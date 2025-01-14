use crate::screens::Screen;
use crate::AppSet;
use avian2d::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::ecs::component::ComponentId;
use bevy::ecs::entity::{EntityHashMap, EntityHashSet};
use bevy::ecs::world::DeferredWorld;
use bevy::pbr::{MaterialPipeline, MaterialPipelineKey};
use bevy::prelude::*;
use bevy::render::mesh::skinning::{SkinnedMesh, SkinnedMeshInverseBindposes};
use bevy::render::mesh::{
    Indices, MeshVertexAttribute, MeshVertexBufferLayoutRef, PrimitiveTopology,
    VertexAttributeValues,
};
use bevy::render::render_resource::{
    AsBindGroup, RenderPipelineDescriptor, ShaderRef, SpecializedMeshPipelineError, VertexFormat,
};
use bevy::sprite::{Material2d, Material2dKey, Material2dPlugin};
use bevy::utils::HashMap;
use bevy::window::PrimaryWindow;
use internal_bevy_auto_plugin_macros::{auto_init_resource, auto_plugin, auto_register_type};
use itertools::Itertools;
use smart_default::SmartDefault;

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "Patty"))]
#[require(Transform)]
#[require(Visibility)]
pub struct Patty;

#[auto_register_type]
#[derive(Component, Debug, Clone, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[component(on_remove=Self::on_remove)]
pub struct PattyJoints {
    physics: Vec<Entity>,
    mesh: Vec<Entity>,
}

impl PattyJoints {
    fn all_maintained_entities(&self) -> Vec<Entity> {
        [].into_iter()
            .chain(self.physics.clone())
            .chain(self.mesh.clone())
            .collect_vec()
    }
    fn on_remove(mut world: DeferredWorld, entity: Entity, _id: ComponentId) {
        let Some(patty_joints) = world.get::<PattyJoints>(entity) else {
            unreachable!();
        };
        for entity in patty_joints.all_maintained_entities() {
            let mut commands = world.commands();
            let Some(cmds) = commands.get_entity(entity) else {
                continue;
            };
            cmds.try_despawn_recursive();
        }
    }
}

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "MeshJointRoot"))]
#[require(Transform)]
#[require(Visibility)]
pub struct MeshJointRoot;

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "MeshJoint"))]
#[require(Transform)]
#[require(Visibility)]
pub struct MeshJoint;

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
pub struct MeshJointIx(usize);

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
pub struct OwnedVertices {
    bottom_left: usize,
    top_left: usize,
    bottom_right: usize,
    top_right: usize,
}

impl From<[usize; 4]> for OwnedVertices {
    fn from(vertices: [usize; 4]) -> Self {
        Self {
            bottom_left: vertices[0],
            top_left: vertices[1],
            bottom_right: vertices[2],
            top_right: vertices[3],
        }
    }
}

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct PattyMesh(Option<Mesh2d>);

impl PattyMesh {
    pub fn clone_handle(&self) -> Option<Mesh2d> {
        self.0.clone()
    }
}

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct PattyMaterial(Option<MeshMaterial2d<CustomMaterial>>);

impl PattyMaterial {
    pub fn clone_handle(&self) -> Option<MeshMaterial2d<CustomMaterial>> {
        self.0.clone()
    }
}

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct SelectedJoint(usize);

#[derive(Asset, AsBindGroup, TypePath, Default, Debug, Clone)]
#[type_path = "burger_flip_game::CustomMaterial"]
pub struct CustomMaterial {
    #[uniform(0)]
    pub color: LinearRgba,
}

const CUSTOM_MATERIAL_PATH: &str = "shaders/custom_shader.wgsl";

impl Material2d for CustomMaterial {
    fn fragment_shader() -> ShaderRef {
        CUSTOM_MATERIAL_PATH.into()
    }

    fn vertex_shader() -> ShaderRef {
        CUSTOM_MATERIAL_PATH.into()
    }

    fn specialize(
        descriptor: &mut RenderPipelineDescriptor,
        layout: &MeshVertexBufferLayoutRef,
        _key: Material2dKey<Self>,
    ) -> Result<(), SpecializedMeshPipelineError> {
        let mut vertex_attributes = Vec::new();
        if layout.0.contains(Mesh::ATTRIBUTE_POSITION) {
            vertex_attributes.push(Mesh::ATTRIBUTE_POSITION.at_shader_location(0));
        }
        if layout.0.contains(ATTRIBUTE_BLEND_COLOR) {
            vertex_attributes.push(ATTRIBUTE_BLEND_COLOR.at_shader_location(1));
        }
        if layout.0.contains(ATTRIBUTE_INSTANCE_OFFSET) {
            vertex_attributes.push(ATTRIBUTE_INSTANCE_OFFSET.at_shader_location(2));
        }
        let vertex_layout = layout.0.get_layout(&vertex_attributes)?;
        descriptor.vertex.buffers = vec![vertex_layout];
        Ok(())
    }
}

const ATTRIBUTE_BLEND_COLOR: MeshVertexAttribute =
    MeshVertexAttribute::new("BlendColor", 111111111_u64, VertexFormat::Float32x4);
pub const ATTRIBUTE_INSTANCE_OFFSET: MeshVertexAttribute =
    MeshVertexAttribute::new("Instance_Offset", 222222222_u64, VertexFormat::Float32x2); // 2D offsets

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_plugins(Material2dPlugin::<CustomMaterial>::default());
    app.add_systems(PreStartup, (init_mesh, init_material).chain());
    app.add_systems(
        FixedUpdate,
        (despawn_patty, patty_respawner)
            .chain()
            .in_set(AppSet::Update),
    );
    app.add_observer(on_patty_add);
    app.add_observer(on_patty_remove);
    app.add_systems(
        Update,
        (select_joint, manipulate_single_joint, update_offsets).chain(),
    );
}

fn update_offsets(
    mut meshes: ResMut<Assets<Mesh>>,
    mesh_q: Query<(&Mesh2d, Entity, &Transform, &GlobalTransform)>,
    parents: Query<&Parent>,
    joints: Query<(Entity, &Transform, &GlobalTransform, &OwnedVertices), With<MeshJoint>>,
) {
    let mut mesh_point_offset_map = EntityHashMap::<HashMap<usize, Vec<Vec2>>>::default();
    for (entity, transform, global_transform, owned_vertices) in joints.iter() {
        let Some((_, mesh_entity, mesh_transform, mesh_global_transform)) = parents
            .iter_ancestors(entity)
            .find_map(|parent| mesh_q.get(parent).ok())
        else {
            panic!("joint {entity} has no ancestor with a mesh");
        };

        let transform_with_offset = global_transform.reparented_to(mesh_global_transform);
        let xy = transform_with_offset.translation.truncate();
        const HALF_WIDTH: f32 = PATTY_PART_WIDTH / 2.0;
        const HALF_HEIGHT: f32 = PATTY_HEIGHT / 2.0;
        const TOP_LEFT: Vec2 = Vec2::new(-HALF_WIDTH, HALF_HEIGHT);
        const BOTTOM_LEFT: Vec2 = Vec2::new(-HALF_WIDTH, -HALF_HEIGHT);
        const TOP_RIGHT: Vec2 = Vec2::new(HALF_WIDTH, HALF_HEIGHT);
        const BOTTOM_RIGHT: Vec2 = Vec2::new(HALF_WIDTH, -HALF_HEIGHT);

        fn rotate_2d(center: Vec2, offset: Vec2, rotation_angle: f32) -> Vec2 {
            let radians = rotation_angle.to_radians();
            let cos_theta = radians.cos();
            let sin_theta = radians.sin();

            let rotated_offset = Vec2::new(
                offset.x * cos_theta - offset.y * sin_theta,
                offset.x * sin_theta + offset.y * cos_theta,
            );

            center + rotated_offset
        }

        let rotation = global_transform.rotation().to_euler(EulerRot::ZXY).0;
        let top_left = rotate_2d(xy, TOP_LEFT, rotation);
        let bottom_left = rotate_2d(xy, BOTTOM_LEFT, rotation);
        let top_right = rotate_2d(xy, TOP_RIGHT, rotation);
        let bottom_right = rotate_2d(xy, BOTTOM_RIGHT, rotation);

        let points = mesh_point_offset_map.entry(mesh_entity).or_default();
        points
            .entry(owned_vertices.top_left)
            .or_default()
            .push(top_left);
        points
            .entry(owned_vertices.top_right)
            .or_default()
            .push(top_right);
        points
            .entry(owned_vertices.bottom_left)
            .or_default()
            .push(bottom_left);
        points
            .entry(owned_vertices.bottom_right)
            .or_default()
            .push(bottom_right);
    }
    for (entity, points) in mesh_point_offset_map {
        let Ok((Mesh2d(mesh_handle), ..)) = mesh_q.get(entity) else {
            unreachable!();
        };
        if let Some(mesh) = meshes.get_mut(mesh_handle) {
            let Some(attr_value) = mesh.attribute_mut(ATTRIBUTE_INSTANCE_OFFSET) else {
                panic!("Mesh does not have Instance_Offset attribute.");
            };
            let VertexAttributeValues::Float32x2(ref mut instance_offsets) = attr_value else {
                panic!("Mesh attribute Instance_Offset is not of type Float32x2.");
            };
            fn into_avg(vectors: Vec<Vec2>) -> Vec2 {
                let len = vectors.len();
                let mut avg = Vec2::ZERO;
                for point in vectors {
                    avg += point;
                }
                avg /= len as f32;
                avg
            }
            for (point, positions) in points {
                instance_offsets[point] = into_avg(positions).to_array();
            }
        }
    }
    println!();
}

fn select_joint(
    mut selected_joint: ResMut<SelectedJoint>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
) {
    for key in keyboard_input.get_just_pressed() {
        let new_selected = match key {
            KeyCode::Digit1 => 0,
            KeyCode::Digit2 => 1,
            KeyCode::Digit3 => 2,
            KeyCode::Digit4 => 3,
            KeyCode::Digit5 => 4,
            KeyCode::Digit6 => 5,
            KeyCode::Digit7 => 6,
            _ => continue,
        };
        log::debug!("selection changed: {} -> {new_selected}", selected_joint.0);
        selected_joint.0 = new_selected;
    }
}

fn manipulate_single_joint(
    selected_joint: Res<SelectedJoint>,
    mut joint_query: Query<(&mut Transform, &MeshJointIx)>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
) {
    for (mut transform, &MeshJointIx(ix)) in joint_query.iter_mut() {
        if ix != selected_joint.0 {
            continue;
        }
        // Replace with the specific joint name
        if keyboard_input.pressed(KeyCode::KeyW) {
            transform.translation.y += 1.0; // Move up
        }
        if keyboard_input.pressed(KeyCode::KeyS) {
            transform.translation.y -= 1.0; // Move down
        }
        if keyboard_input.pressed(KeyCode::KeyA) {
            transform.translation.x -= 1.0; // Move left
        }
        if keyboard_input.pressed(KeyCode::KeyD) {
            transform.translation.x += 1.0; // Move right
        }
    }
}

#[derive(Debug, SmartDefault)]
pub struct SpawnPatty {
    pub pos: Vec2,
    #[default(Vec2::splat(1.0))]
    pub scale: Vec2,
}

impl Command for SpawnPatty {
    fn apply(self, world: &mut World) {
        let _ = world.run_system_cached_with(spawn_patty, self);
    }
}

const PATTY_PARTS: usize = 2;
const PATTY_WIDTH: f32 = 100.0;
const PATTY_PART_WIDTH: f32 = PATTY_WIDTH / PATTY_PARTS as f32;
const PATTY_HEIGHT: f32 = 20.0;

fn spawn_patty(In(config): In<SpawnPatty>, mut commands: Commands) {
    let joints = create_joint_entities(
        MeshOptions {
            segments: PATTY_PARTS,
            width: PATTY_WIDTH,
            height: PATTY_HEIGHT,
        },
        &mut commands,
    );

    let mut patty_joints = PattyJoints {
        physics: Vec::with_capacity(joints.len()),
        mesh: joints.clone(),
    };

    for &joint in patty_joints.mesh.iter().skip(1) {
        commands.entity(joint).insert((
            RigidBody::Dynamic,
            NoAutoCenterOfMass,
            Collider::rectangle(PATTY_PART_WIDTH, PATTY_HEIGHT),
        ));
    }

    for (&left, &right) in joints.iter().tuple_windows() {
        let half_width = PATTY_PART_WIDTH * config.scale / 2.0;
        let joint = commands
            .spawn((
                Name::new(format!("PattyJoint({left}, {right})")),
                RevoluteJoint::new(left, right)
                    .with_local_anchor_1(Vec2::X * half_width)
                    .with_local_anchor_2(Vec2::NEG_X * half_width)
                    .with_compliance(0.0000001),
                StateScoped(Screen::Gameplay),
            ))
            .id();
        patty_joints.physics.push(joint);
    }

    let first_joint = *joints.first().expect("no joints!");
    let translation = config.pos.extend(0.0);
    let transform_scale = config.scale.extend(1.0);
    commands
        .spawn((
            Patty,
            patty_joints,
            StateScoped(Screen::Gameplay),
            Transform::from_translation(translation).with_scale(transform_scale),
        ))
        .add_child(first_joint);
}

fn on_patty_add(
    trigger: Trigger<OnAdd, Patty>,
    mut commands: Commands,
    patty_mesh: Res<PattyMesh>,
    pan_material: Res<PattyMaterial>,
) {
    let mesh = patty_mesh
        .clone_handle()
        .expect("Patty mesh not initialized");
    let material = pan_material
        .clone_handle()
        .expect("Patty material not initialized");

    commands.entity(trigger.entity()).insert((mesh, material));
}

fn on_patty_remove(
    trigger: Trigger<OnRemove, Patty>,
    mut commands: Commands,
    patties: Query<&PattyJoints, With<Patty>>,
) {
    let entity = trigger.entity();
    let Ok(patty_joints) = patties.get(entity) else {
        return;
    };

    for entity in patty_joints.all_maintained_entities() {
        let Some(cmds) = commands.get_entity(entity) else {
            continue;
        };
        cmds.try_despawn_recursive();
    }
}

fn patty_respawner(
    mut removed_patties: RemovedComponents<Patty>,
    screen: Res<State<Screen>>,
    mut commands: Commands,
    patties: Query<(), With<Patty>>,
) {
    let mut should_spawn = false;
    for removed_patty in removed_patties.read() {
        log::debug!("Patty removed {removed_patty}");
        if screen.get() != &Screen::Gameplay {
            log::debug!("Not in Gameplay screen");
            return;
        }
        if !patties.is_empty() {
            log::debug!("Patties still exist");
            return;
        }
        should_spawn = true;
    }

    if !should_spawn {
        return;
    }

    log::debug!("Spawn patty");
    commands.queue(SpawnPatty {
        pos: Vec2::Y * 100.0 + Vec2::X * -20.0,
        scale: Vec2::splat(1.5),
    });
}

fn init_mesh(mut pan_mesh: ResMut<PattyMesh>, mut meshes: ResMut<Assets<Mesh>>) {
    if pan_mesh.0.is_some() {
        return;
    }
    let handle = meshes.add(create_horizontal_segmented_rectangle_with_joints(
        MeshOptions {
            segments: PATTY_PARTS,
            width: PATTY_WIDTH,
            height: PATTY_HEIGHT,
        },
    ));
    pan_mesh.0 = Some(Mesh2d(handle));
}

fn init_material(
    mut pan_material: ResMut<PattyMaterial>,
    mut materials: ResMut<Assets<CustomMaterial>>,
) {
    if pan_material.0.is_some() {
        return;
    }
    let handle = materials.add(CustomMaterial {
        color: Color::Srgba(bevy::color::palettes::tailwind::AMBER_950).to_linear(),
        ..default()
    });
    pan_material.0 = Some(MeshMaterial2d(handle));
}

fn despawn_patty(
    mut commands: Commands,
    window: Single<&Window, With<PrimaryWindow>>,
    out_of_bounds_query: Query<(Entity, &Transform), With<Patty>>,
) {
    let size = window.size() + 256.0;
    let half_size = size / 2.0;
    let mut entities_to_despawn = EntityHashSet::default();
    for (entity, transform) in &out_of_bounds_query {
        if entities_to_despawn.contains(&entity) {
            continue;
        }
        let position = transform.translation.xy();
        if position.y < -half_size.y || position.x.abs() > half_size.x * 2.0 {
            entities_to_despawn.insert(entity);
        }
    }
    for entity in entities_to_despawn {
        commands.entity(entity).try_despawn_recursive();
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct MeshOptions {
    segments: usize,
    width: f32,
    height: f32,
}

impl MeshOptions {
    fn segment_width(&self) -> f32 {
        self.width / self.segments as f32
    }
}

fn create_horizontal_segmented_rectangle_with_joints(mesh_options: MeshOptions) -> Mesh {
    let segment_width = mesh_options.segment_width();
    let MeshOptions {
        segments,
        width,
        height,
    } = mesh_options;

    // Vertex positions for the mesh
    let mut positions = Vec::new();
    // UV coordinates
    let mut uvs = Vec::new();
    // Normals
    let mut normals = Vec::new();
    // Indices for triangles
    let mut indices = Vec::new();

    for i in 0..=segments {
        let x = i as f32 * segment_width - width / 2.0; // Center the rectangle on the X-axis
        let v = i as f32 / segments as f32; // Map texture width from 0.0 to 1.0

        // Add two vertices for the segment: top and bottom
        positions.push([x, -height / 2.0, 0.0]); // Bottom vertex
        positions.push([x, height / 2.0, 0.0]); // Top vertex

        // UV coordinates (full texture mapping)
        uvs.push([v, 1.0]); // Right edge of the texture
        uvs.push([v, 0.0]); // Left edge of the texture

        // Normals point forward in the Z direction
        normals.push([0.0, 0.0, 1.0]);
        normals.push([0.0, 0.0, 1.0]);

        // Add triangles if we're not at the far right
        if i > 0 {
            let base_index = (i - 1) * 2;
            indices.extend_from_slice(&[
                base_index as u16,
                (base_index + 1) as u16,
                (base_index + 3) as u16,
                base_index as u16,
                (base_index + 3) as u16,
                (base_index + 2) as u16,
            ]);
        }
    }

    for (ix, pos) in positions.iter().enumerate() {
        println!("{ix} {pos:?}");
    }

    // Create the mesh
    Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
    )
    .with_inserted_attribute(
        ATTRIBUTE_BLEND_COLOR,
        VertexAttributeValues::Float32x4(vec![[0.0, 0.0, 0.0, 1.0]; positions.len()]),
    )
    .with_inserted_attribute(
        ATTRIBUTE_INSTANCE_OFFSET,
        VertexAttributeValues::Float32x2(vec![[0.0, 0.0]; positions.len()]),
    )
    .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
    .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
    .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
    .with_inserted_indices(Indices::U16(indices))
}

fn create_joint_entities(mesh_options: MeshOptions, commands: &mut Commands) -> Vec<Entity> {
    #[derive(Debug)]
    enum Parent {
        Root(Entity),
        LR(Entity, Entity),
    }
    impl Parent {
        fn set_left(&mut self, left: Entity) {
            *self = match self {
                Parent::Root(root) => Self::LR(left, *root),
                Parent::LR(_, right) => Self::LR(left, *right),
            }
        }
        fn set_right(&mut self, right: Entity) {
            *self = match self {
                Parent::Root(root) => Self::LR(*root, right),
                Parent::LR(left, _) => Self::LR(*left, right),
            }
        }
    }

    #[derive(Debug)]
    enum Direction {
        Left,
        Right,
    }
    impl Direction {
        fn to_sign(&self) -> i32 {
            match self {
                Self::Left => 1,
                Self::Right => -1,
            }
        }
        fn from_ix(ix: usize) -> Self {
            if ix % 2 == 0 {
                Self::Left
            } else {
                Self::Right
            }
        }
    }

    let segment_width = mesh_options.segment_width();
    let translation = |ix| {
        let direction = Direction::from_ix(ix);
        let offset = if matches!(direction, Direction::Right) {
            -1
        } else {
            0
        };
        let layer = (ix as i32 + offset) / 2;
        let half_size = segment_width / 2.0 * direction.to_sign() as f32;
        Vec3::X * layer as f32 * half_size + Vec3::X * half_size
    };

    let MeshOptions {
        segments, width, ..
    } = mesh_options;

    // Create joint entities and attach them to a parent for easy animation
    let root = commands.spawn((MeshJointRoot, MeshJointIx(0))).id();
    let mut parent = Parent::Root(root);
    let mut joints = {
        let mut joints = Vec::with_capacity(segments + 1);
        joints.push(root);
        joints
    };
    joints.extend(
        (0..segments)
            .map(|ix| {
                let joint = commands
                    .spawn((
                        MeshJoint,
                        MeshJointIx(ix + 1),
                        OwnedVertices::from(vertex_indices_for_joint(ix, segments)),
                        // Transform::from_translation(translation(ix)),
                    ))
                    .id();
                {
                    let direction = Direction::from_ix(ix);
                    {
                        let parent = match parent {
                            Parent::Root(root) => root,
                            Parent::LR(left, right) => match direction {
                                Direction::Left => left,
                                Direction::Right => right,
                            },
                        };
                        commands.entity(parent).add_child(joint);
                    }
                    match direction {
                        Direction::Left => parent.set_left(joint),
                        Direction::Right => parent.set_right(joint),
                    }
                }
                joint
            })
            .collect_vec(),
    );

    joints
}

fn vertex_indices_for_joint(joint_skeleton_ix: usize, segments: usize) -> [usize; 4] {
    // Determine if the segment is on the left or right
    let is_left = joint_skeleton_ix % 2 == 0;

    // Calculate the segment number (0-based index for each direction)
    let segment_number = joint_skeleton_ix / 2;

    // Calculate the base vertex index for this joint
    let base_index = if is_left {
        segments - (segment_number + 1) * 2 // Left segments go backward
    } else {
        segments + segment_number * 2 // Right segments go forward
    };

    // Return the 4 vertex indices for this segment
    [
        base_index,     // Bottom-left
        base_index + 1, // Top-left
        base_index + 2, // Bottom-right
        base_index + 3, // Top-right
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_vertex_indices_for_joint() {
        for ix in 0..PATTY_PARTS {
            println!("{ix} {:?}", vertex_indices_for_joint(ix, PATTY_PARTS));
        }
        let sets = (0..2)
            .map(|ix| vertex_indices_for_joint(ix, 2))
            .collect::<Vec<_>>();
        assert_eq!(sets, vec![[0, 1, 2, 3], [2, 3, 4, 5]]);
        let unique = (0..2)
            .flat_map(|ix| vertex_indices_for_joint(ix, 2))
            .collect::<std::collections::HashSet<_>>()
            .len();
        let mesh = create_horizontal_segmented_rectangle_with_joints(MeshOptions {
            segments: 2,
            width: 1.0,
            height: 1.0,
        });
        assert_eq!(unique, mesh.count_vertices());
    }
}
