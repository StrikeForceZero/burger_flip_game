use crate::screens::Screen;
use crate::AppSet;
use avian2d::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::ecs::component::ComponentId;
use bevy::ecs::entity::EntityHashSet;
use bevy::ecs::world::DeferredWorld;
use bevy::prelude::*;
use bevy::render::mesh::skinning::{SkinnedMesh, SkinnedMeshInverseBindposes};
use bevy::render::mesh::{Indices, PrimitiveTopology, VertexAttributeValues};
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
pub struct PattyMaterial(Option<MeshMaterial2d<ColorMaterial>>);

impl PattyMaterial {
    pub fn clone_handle(&self) -> Option<MeshMaterial2d<ColorMaterial>> {
        self.0.clone()
    }
}

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct SelectedJoint(usize);

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_systems(PreStartup, (init_mesh, init_material).chain());
    app.add_systems(
        FixedUpdate,
        (despawn_patty, patty_respawner)
            .chain()
            .in_set(AppSet::Update),
    );
    app.add_observer(on_patty_add);
    app.add_observer(on_patty_remove);
    app.add_systems(Update, (select_joint, manipulate_single_joint).chain());
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

const PATTY_PARTS: usize = 6;
const PATTY_WIDTH: f32 = 100.0;
const PATTY_PART_WIDTH: f32 = PATTY_WIDTH / PATTY_PARTS as f32;
const PATTY_HEIGHT: f32 = 20.0;

fn spawn_patty(
    In(config): In<SpawnPatty>,
    mut commands: Commands,
    mut skinned_mesh_inverse_bindposes_assets: ResMut<Assets<SkinnedMeshInverseBindposes>>,
) {
    let skinned_mesh = create_joint_entities(
        MeshOptions {
            segments: PATTY_PARTS,
            width: PATTY_WIDTH,
            height: PATTY_HEIGHT,
        },
        &mut commands,
        &mut skinned_mesh_inverse_bindposes_assets,
    );

    let mut patty_joints = PattyJoints {
        physics: Vec::with_capacity(skinned_mesh.joints.len()),
        mesh: skinned_mesh.joints.clone(),
    };

    for &joint in patty_joints.mesh.iter().skip(1) {
        commands.entity(joint).insert((
            RigidBody::Dynamic,
            NoAutoCenterOfMass,
            Collider::rectangle(PATTY_PART_WIDTH, PATTY_HEIGHT),
        ));
    }

    for (&left, &right) in skinned_mesh.joints.iter().tuple_windows() {
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

    let first_joint = *skinned_mesh.joints.first().expect("no joints!");
    let translation = config.pos.extend(0.0);
    let transform_scale = config.scale.extend(1.0);
    commands
        .spawn((
            Patty,
            patty_joints,
            StateScoped(Screen::Gameplay),
            Transform::from_translation(translation).with_scale(transform_scale),
            skinned_mesh,
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
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    if pan_material.0.is_some() {
        return;
    }
    let handle = materials.add(ColorMaterial::from_color(
        bevy::color::palettes::tailwind::AMBER_950,
    ));
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
    // Joint indices
    let mut joint_indices = Vec::new();
    // Joint weights
    let mut joint_weights = Vec::new();
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

        // Each vertex is affected by two joints: the current and the next

        if i == 0 {
            // First vertex is fully influenced by the first joint
            joint_indices.push([0, 0, 0, 0]);
            joint_indices.push([0, 0, 0, 0]);
            joint_weights.push([1.0, 0.0, 0.0, 0.0]);
            joint_weights.push([1.0, 0.0, 0.0, 0.0]);
        } else if i == segments {
            // Last vertex is fully influenced by the last joint
            joint_indices.push([segments as u16, 0, 0, 0]);
            joint_indices.push([segments as u16, 0, 0, 0]);
            joint_weights.push([1.0, 0.0, 0.0, 0.0]);
            joint_weights.push([1.0, 0.0, 0.0, 0.0]);
        } else {
            // Vertices between joints are influenced by two joints
            joint_indices.push([i as u16, (i + 1) as u16, 0, 0]);
            joint_indices.push([i as u16, (i + 1) as u16, 0, 0]);
            joint_weights.push([0.5, 0.5, 0.0, 0.0]);
            joint_weights.push([0.5, 0.5, 0.0, 0.0]);
        }

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

    // Create the mesh
    Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
    )
    .with_inserted_attribute(Mesh::ATTRIBUTE_POSITION, positions)
    .with_inserted_attribute(Mesh::ATTRIBUTE_UV_0, uvs)
    .with_inserted_attribute(Mesh::ATTRIBUTE_NORMAL, normals)
    .with_inserted_attribute(
        Mesh::ATTRIBUTE_JOINT_INDEX,
        VertexAttributeValues::Uint16x4(joint_indices),
    )
    .with_inserted_attribute(Mesh::ATTRIBUTE_JOINT_WEIGHT, joint_weights)
    .with_inserted_indices(Indices::U16(indices))
}

fn create_joint_entities(
    mesh_options: MeshOptions,
    commands: &mut Commands,
    skinned_mesh_inverse_bindposes_assets: &mut ResMut<Assets<SkinnedMeshInverseBindposes>>,
) -> SkinnedMesh {
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

    // Create the inverse bindpose matrices for the joints
    let inverse_bindposes = skinned_mesh_inverse_bindposes_assets.add(
        (0..segments)
            .map(|ix| Mat4::from_translation(translation(ix)))
            .collect_vec(),
    );

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
                        Transform::from_translation(translation(ix)),
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

    SkinnedMesh {
        inverse_bindposes,
        joints,
    }
}
