use crate::game::bun::BunAreaSensor;
use crate::game::pan::HasPattyInArea;
use crate::game::{PattyLanded, PattyOutOfBounds};
use crate::screens::Screen;
use crate::AppSet;
use avian2d::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::ecs::component::ComponentId;
use bevy::ecs::entity::{EntityHashMap, EntityHashSet};
use bevy::ecs::system::SystemParam;
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
#[derive(Component, Debug, Clone, PartialEq, Eq, Reflect)]
#[reflect(Component)]
#[component(on_remove=Self::on_remove)]
pub struct PattyStructure {
    root: Entity,
    physic_joints: Vec<Entity>,
    mesh_joints: EntityHashMap<(Entity, Entity)>,
    mesh_segments: Vec<Entity>,
}

impl PattyStructure {
    fn new(root: Entity) -> Self {
        Self {
            root: root,
            physic_joints: Default::default(),
            mesh_joints: Default::default(),
            mesh_segments: Default::default(),
        }
    }
    fn all_maintained_entities(&self) -> Vec<Entity> {
        [].into_iter()
            .chain(std::iter::once(self.root))
            .chain(self.physic_joints.clone())
            .chain(self.mesh_segments.clone())
            .collect_vec()
    }
    fn on_remove(mut world: DeferredWorld, entity: Entity, _id: ComponentId) {
        let Some(patty_joints) = world.get::<PattyStructure>(entity) else {
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
#[require(Transform)]
#[require(Visibility)]
pub struct MeshSegmentRoot;

#[auto_register_type]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub enum Direction {
    Left,
    Right,
}

impl Direction {
    fn to_sign(&self) -> i32 {
        match self {
            Self::Left => -1,
            Self::Right => 1,
        }
    }
    fn is_left(&self) -> bool {
        match self {
            Self::Left => true,
            Self::Right => false,
        }
    }
    fn is_right(&self) -> bool {
        match self {
            Self::Left => false,
            Self::Right => true,
        }
    }
}

#[auto_register_type]
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "MeshJoint"))]
#[require(Transform)]
#[require(Visibility)]
pub struct MeshJoint {
    left: Entity,
    right: Entity,
    direction: Direction,
}

impl MeshJoint {
    fn new(parent: Entity, next: Entity, direction: Direction) -> Self {
        match direction {
            Direction::Left => Self {
                left: next,
                right: parent,
                direction,
            },
            Direction::Right => Self {
                left: parent,
                right: next,
                direction,
            },
        }
    }
    fn left(&self) -> Entity {
        self.left
    }
    fn right(&self) -> Entity {
        self.right
    }
    fn lr(&self) -> (Entity, Entity) {
        (self.left, self.right)
    }
    fn direction(&self) -> Direction {
        self.direction
    }
    fn parent(&self) -> Entity {
        match self.direction {
            Direction::Left => self.right,
            Direction::Right => self.left,
        }
    }
    fn next(&self) -> Entity {
        match self.direction {
            Direction::Left => self.left,
            Direction::Right => self.right,
        }
    }
}

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "MeshSegment"))]
#[require(Transform)]
#[require(Visibility)]
pub struct MeshSegment;

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
pub struct MeshSegmentIx(usize);

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
pub struct OwnedVertices {
    top_right: usize,
    top_left: usize,
    bottom_left: usize,
    bottom_right: usize,
}

impl OwnedVertices {
    const PATTY_SEGMENT: Self = Self {
        top_right: 0,
        top_left: 1,
        bottom_left: 2,
        bottom_right: 3,
    };
}

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct PattyMesh(Option<Mesh>);

impl PattyMesh {
    pub fn clone_mesh(&self) -> Option<Mesh> {
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

#[derive(SystemParam)]
struct PattyMeshMaterialSystemParam<'w> {
    meshes: ResMut<'w, Assets<Mesh>>,
    mesh: Res<'w, PattyMesh>,
    material: Res<'w, PattyMaterial>,
}

impl PattyMeshMaterialSystemParam<'_> {
    pub fn mesh(&self) -> Mesh {
        self.mesh.0.clone().expect("Patty mesh not initialized")
    }
    pub fn mesh_2d(&mut self) -> Mesh2d {
        let mesh = self.mesh();
        Mesh2d(self.meshes.add(mesh))
    }
    pub fn material(&self) -> MeshMaterial2d<CustomMaterial> {
        self.material
            .0
            .clone()
            .expect("Patty material not initialized")
    }
}

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct SelectedSegment(usize);

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
        let vertex_layout = layout.0.get_layout(&vertex_attributes)?;
        descriptor.vertex.buffers = vec![vertex_layout];
        Ok(())
    }
}

const ATTRIBUTE_BLEND_COLOR: MeshVertexAttribute =
    MeshVertexAttribute::new("BlendColor", 111111111_u64, VertexFormat::Float32x4);

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_plugins(Material2dPlugin::<CustomMaterial>::default());
    app.add_systems(PreStartup, (init_mesh, init_material).chain());
    app.add_systems(
        FixedUpdate,
        (despawn_patty, check_patty_bun, patty_respawner)
            .chain()
            .in_set(AppSet::Update)
            .run_if(Screen::run_if_is_gameplay),
    );
    app.add_observer(on_patty_remove);
    app.add_systems(
        Update,
        (select_segment, manipulate_single_segment, update_offsets).chain(),
    );
}

fn check_patty_bun(
    mut commands: Commands,
    mut collision_event_reader: EventReader<Collision>,
    sensor: Single<Entity, With<BunAreaSensor>>,
    patties: Query<(Entity, &RigidBody, &LinearVelocity), With<Patty>>,
    children: Query<&Children>,
    rigid_bodies: Query<(Entity, &LinearVelocity), With<RigidBody>>,
) {
    if patties.iter().map(|(_, r, ..)| r).all(RigidBody::is_static) {
        return;
    }
    let mut scored = false;
    for Collision(contacts) in collision_event_reader.read() {
        if (*sensor == contacts.entity1 && patties.contains(contacts.entity2)
            || (*sensor == contacts.entity2 && patties.contains(contacts.entity1)))
        {
            let Some((entity, rigid_body, linear_velocity)) = patties
                .get(contacts.entity1)
                .ok()
                .or_else(|| patties.get(contacts.entity2).ok())
            else {
                panic!("Patty not found")
            };
            if linear_velocity.length() < 3.0 && rigid_body.is_dynamic() {
                let mut to_freeze = vec![entity];
                for (entity, linear_velocity) in children
                    .iter_descendants(entity)
                    .filter_map(|child| rigid_bodies.get(child).ok())
                {
                    if linear_velocity.length() >= 5.0 {
                        to_freeze.clear();
                        break;
                    }
                    to_freeze.push(entity);
                }
                if !to_freeze.is_empty() {
                    scored = true;
                }
                for entity in to_freeze {
                    commands.entity(entity).insert(RigidBody::Static);
                }
            }
        }
    }
    // TODO: doesn't handle multiple patties
    if scored {
        commands.send_event(PattyLanded);
    }
}

fn update_offsets(
    mut meshes: ResMut<Assets<Mesh>>,
    root: Query<&Transform, With<MeshSegmentRoot>>,
    parents: Query<&Parent>,
    mesh_segment_q: Query<(&Mesh2d, &GlobalTransform), With<MeshSegment>>,
    joints_q: Query<
        (
            Entity,
            &Mesh2d,
            &MeshJoint,
            &OwnedVertices,
            &GlobalTransform,
        ),
        (With<MeshJoint>, Without<MeshSegment>),
    >,
) {
    for (entity, Mesh2d(mesh_handle), mesh_joint, owned_vertices, global_transform) in
        joints_q.iter()
    {
        let Ok((_, left_global_transform)) = mesh_segment_q.get(mesh_joint.left) else {
            panic!("Left mesh segment not found");
        };
        let Ok((_, right_global_transform)) = mesh_segment_q.get(mesh_joint.right) else {
            panic!("Right mesh segment not found");
        };
        let left_local_transform = left_global_transform.reparented_to(global_transform);
        let right_local_transform = right_global_transform.reparented_to(global_transform);
        const HALF_WIDTH: f32 = PATTY_PART_WIDTH / 2.0;
        const HALF_HEIGHT: f32 = PATTY_HEIGHT / 2.0;
        const TOP_LEFT: Vec3 = Vec3::new(-HALF_WIDTH, HALF_HEIGHT, 0.0);
        const BOTTOM_LEFT: Vec3 = Vec3::new(-HALF_WIDTH, -HALF_HEIGHT, 0.0);
        const TOP_RIGHT: Vec3 = Vec3::new(HALF_WIDTH, HALF_HEIGHT, 0.0);
        const BOTTOM_RIGHT: Vec3 = Vec3::new(HALF_WIDTH, -HALF_HEIGHT, 0.0);

        let top_left = right_local_transform.transform_point(TOP_LEFT);

        let bottom_left = right_local_transform.transform_point(BOTTOM_LEFT);

        let top_right = left_local_transform.transform_point(TOP_RIGHT);

        let bottom_right = left_local_transform.transform_point(BOTTOM_RIGHT);

        let Some(mesh) = meshes.get_mut(mesh_handle) else {
            panic!("MeshJoint mesh not found");
        };
        let Some(attr_value) = mesh.attribute_mut(Mesh::ATTRIBUTE_POSITION) else {
            panic!("Mesh does not have ATTRIBUTE_POSITION attribute.");
        };
        let VertexAttributeValues::Float32x3(ref mut instance_offsets) = attr_value else {
            panic!("Mesh attribute Instance_Offset is not of type Float32x2.");
        };

        instance_offsets[owned_vertices.bottom_left] = bottom_left.to_array();
        instance_offsets[owned_vertices.top_left] = top_left.to_array();
        instance_offsets[owned_vertices.bottom_right] = bottom_right.to_array();
        instance_offsets[owned_vertices.top_right] = top_right.to_array();
    }
}

fn select_segment(
    mut selected_joint: ResMut<SelectedSegment>,
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

fn manipulate_single_segment(
    selected_segment: Res<SelectedSegment>,
    mut segment_query: Query<(&mut Transform, &MeshSegmentIx)>,
    keyboard_input: Res<ButtonInput<KeyCode>>,
) {
    for (mut transform, &MeshSegmentIx(ix)) in segment_query.iter_mut() {
        if ix != selected_segment.0 {
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

const PATTY_PARTS: usize = 5;
const PATTY_PART_WIDTH: f32 = 15.0;
const PATTY_HEIGHT: f32 = 15.0;
const PATTY_JOINT_WIDTH: f32 = PATTY_HEIGHT / 5.0;
const PATTY_WIDTH: f32 =
    PATTY_PART_WIDTH * PATTY_PARTS as f32 + PATTY_JOINT_WIDTH * (PATTY_PARTS - 1) as f32;

fn spawn_patty(
    In(config): In<SpawnPatty>,
    mut commands: Commands,
    mut pmmmsp: PattyMeshMaterialSystemParam,
) {
    let mut patty_structure = create_segment_and_joint_entities(
        MeshOptions {
            segments: PATTY_PARTS,
            width: PATTY_WIDTH,
            height: PATTY_HEIGHT,
        },
        &mut commands,
    )
    .unwrap_or_else(|err| {
        panic!("Failed to create patty: {:?}", err);
    });

    for &entity in patty_structure
        .mesh_segments
        .iter()
        .chain(std::iter::once(&patty_structure.root))
    {
        commands.entity(entity).insert((
            RigidBody::Dynamic,
            NoAutoCenterOfMass,
            Collider::rectangle(PATTY_PART_WIDTH, PATTY_HEIGHT),
            pmmmsp.mesh_2d(),
            pmmmsp.material(),
        ));
    }

    // TODO: is squared really the right math?
    let joint_edge_offset = PATTY_JOINT_WIDTH * config.scale.powf(2.0);
    patty_structure.physic_joints = Vec::with_capacity(patty_structure.mesh_joints.len() * 2);
    for (&entity, &(left, right)) in patty_structure.mesh_joints.iter() {
        commands.entity(entity).insert((
            RigidBody::Dynamic,
            NoAutoCenterOfMass,
            Collider::circle(PATTY_JOINT_WIDTH / 2.0),
            pmmmsp.mesh_2d(),
            pmmmsp.material(),
        ));
        for (left, right) in [(left, entity), (entity, right)] {
            let physics_joint = commands
                .spawn((
                    Name::new(format!("PattyJoint({left}, {right})")),
                    RevoluteJoint::new(left, right)
                        .with_local_anchor_1(Vec2::X * joint_edge_offset)
                        .with_local_anchor_2(Vec2::NEG_X * joint_edge_offset)
                        .with_compliance(0.0000001),
                    StateScoped(Screen::Gameplay),
                ))
                .id();
            patty_structure.physic_joints.push(physics_joint);
        }
    }

    let translation = config.pos.extend(0.0);
    let transform_scale = config.scale.extend(1.0);
    commands.entity(patty_structure.root).insert((
        Name::new("Patty"),
        Patty,
        patty_structure,
        StateScoped(Screen::Gameplay),
        Transform::from_translation(translation).with_scale(transform_scale),
    ));
}

fn on_patty_remove(
    trigger: Trigger<OnRemove, Patty>,
    mut commands: Commands,
    patties: Query<&PattyStructure, With<Patty>>,
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
    mut commands: Commands,
    has_patty_in_area: Res<HasPattyInArea>,
    patties: Query<(Entity, &RigidBody), With<Patty>>,
) {
    let mut should_spawn = None;
    for removed_patty in removed_patties.read() {
        log::trace!("Patty removed {removed_patty}");
        should_spawn = Some(true);
    }

    if has_patty_in_area.in_area() {
        log::trace!("Patty still in pan area");
        should_spawn = Some(false);
    } else {
        for (entity, &rigid_body) in patties.iter() {
            if rigid_body != RigidBody::Static {
                log::trace!("At least one active patty remains {entity}");
                should_spawn = Some(false);
                break;
            }
        }
    }

    if should_spawn == Some(false) {
        return;
    }

    log::debug!("Spawn patty");
    commands.queue(SpawnPatty {
        pos: Vec2::Y * 100.0 + Vec2::X * -20.0,
        scale: Vec2::splat(1.5),
    });
}

fn init_mesh(mut pan_mesh: ResMut<PattyMesh>) {
    if pan_mesh.0.is_some() {
        return;
    }
    pan_mesh.0 = Some(create_patty_part_mesh(PATTY_PART_WIDTH, PATTY_HEIGHT));
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
        commands.send_event(PattyOutOfBounds);
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

fn create_patty_part_mesh(width: f32, height: f32) -> Mesh {
    Rectangle::new(width, height)
        .mesh()
        .build()
        .with_inserted_attribute(
            ATTRIBUTE_BLEND_COLOR,
            VertexAttributeValues::Float32x4(vec![[1.0, 1.0, 1.0, 1.0]; 4]),
        )
}

fn create_segment_and_joint_entities(
    mesh_options: MeshOptions,
    commands: &mut Commands,
) -> Result<PattyStructure, ()> {
    #[derive(Debug, Copy, Clone)]
    struct JointParams {
        parent: Entity,
        child: Option<Entity>,
        direction: Direction,
    }

    impl TryFrom<JointParams> for MeshJoint {
        type Error = ();
        fn try_from(value: JointParams) -> Result<Self, Self::Error> {
            let Some(child) = value.child else {
                return Err(());
            };
            Ok(Self::new(value.parent, child, value.direction))
        }
    }

    #[derive(Debug, Copy, Clone)]
    struct CreatedJoint {
        entity: Entity,
        mesh_joint: MeshJoint,
    }

    impl TryFrom<(Entity, JointParams)> for CreatedJoint {
        type Error = ();
        fn try_from(value: (Entity, JointParams)) -> Result<Self, Self::Error> {
            let (entity, value) = value;
            Ok(CreatedJoint {
                entity,
                mesh_joint: MeshJoint::try_from(value)?,
            })
        }
    }

    #[derive(Debug, Copy, Clone)]
    struct ExtendResult {
        se: SE,
        created_joint: Option<CreatedJoint>,
        created_segment: Option<Entity>,
    }

    impl ExtendResult {
        fn new(se: SE) -> Self {
            Self {
                se,
                created_joint: None,
                created_segment: None,
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    enum SE {
        Segment(Entity),
        Joint(Entity, JointParams),
    }

    impl SE {
        fn entity(&self) -> Entity {
            match *self {
                SE::Segment(entity) => entity,
                SE::Joint(entity, _) => entity,
            }
        }
        fn root(root: Entity, entity: Entity, direction: Direction) -> Self {
            Self::Joint(
                entity,
                JointParams {
                    parent: root,
                    child: None,
                    direction,
                },
            )
        }
        fn extend(self, commands: &mut Commands, direction: Direction) -> Result<ExtendResult, ()> {
            Ok(match self {
                Self::Segment(parent) => {
                    let joint = commands.spawn_empty().id();
                    commands.entity(parent).add_child(joint);
                    ExtendResult::new(Self::Joint(
                        joint,
                        JointParams {
                            parent,
                            child: None,
                            direction,
                        },
                    ))
                }
                Self::Joint(entity, mut params) => {
                    if direction != params.direction {
                        panic!("mismatched direction!");
                    }
                    let segment = commands.spawn_empty().id();
                    commands.entity(entity).add_child(segment);
                    params.child = Some(segment);

                    ExtendResult {
                        se: Self::Segment(segment),
                        created_joint: Some(CreatedJoint::try_from((entity, params))?),
                        created_segment: Some(segment),
                    }
                }
            })
        }
    }

    #[derive(Debug)]
    enum Parent {
        Root(Entity),
        LR(SE, SE),
    }
    impl Parent {
        fn init(commands: &mut Commands, root: Entity) -> Result<Self, ()> {
            let left = commands.spawn_empty().id();
            let right = commands.spawn_empty().id();
            commands.entity(root).add_child(left).add_child(right);
            Ok(Self::LR(
                SE::root(root, left, Direction::Left),
                SE::root(root, right, Direction::Right),
            ))
        }
        fn extend_left(&mut self, commands: &mut Commands) -> Result<ExtendResult, ()> {
            Ok(match self {
                Self::LR(left, right) => {
                    let res = left.extend(commands, Direction::Left)?;
                    *self = Self::LR(res.se, *right);
                    res
                }
                Self::Root(_) => panic!("not initialized"),
            })
        }
        fn extend_right(&mut self, commands: &mut Commands) -> Result<ExtendResult, ()> {
            Ok(match self {
                Self::LR(left, right) => {
                    let res = right.extend(commands, Direction::Right)?;
                    *self = Self::LR(*left, res.se);
                    res
                }
                Self::Root(_) => panic!("not initialized"),
            })
        }
        fn parent(&self, direction: Direction) -> Entity {
            match *self {
                Parent::Root(root) => root,
                Parent::LR(left, right) => match direction {
                    Direction::Left => left.entity(),
                    Direction::Right => right.entity(),
                },
            }
        }
    }

    fn dir_from_ix(ix: usize) -> Option<Direction> {
        if ix == 0 {
            return None;
        }
        let direction = if ix % 2 == 0 {
            Direction::Left
        } else {
            Direction::Right
        };
        Some(direction)
    }

    let segment_width = mesh_options.segment_width();
    let MeshOptions {
        segments: segment_count,
        ..
    } = mesh_options;

    // Create segment entities and attach them to a parent for easy animation
    let root = commands.spawn((MeshSegmentRoot, MeshSegmentIx(0))).id();
    let mut patty_structure = PattyStructure::new(root);

    let mut segments = HashMap::<Direction, Vec<Entity>>::new();
    let mut joints = HashMap::<Direction, Vec<Entity>>::new();

    let mut on_seg_created = |commands: &mut Commands, entity: Entity, ix: usize| {
        patty_structure.mesh_segments.push(entity);
        commands.entity(entity).insert((
            MeshSegment,
            MeshSegmentIx(ix),
            OwnedVertices::PATTY_SEGMENT,
        ));
    };

    let mut on_joint_created = |commands: &mut Commands, entity: Entity, mesh_joint: MeshJoint| {
        let (left, right) = mesh_joint.lr();
        let insert_result = patty_structure.mesh_joints.insert(entity, (left, right));
        debug_assert!(insert_result.is_none(), "impossible double insertion");
        commands.entity(entity).insert((
            Name::new(format!("MeshJoint({left}, {right})",)),
            mesh_joint,
            OwnedVertices::PATTY_SEGMENT,
        ));
    };

    let mut parent = Parent::init(commands, root)?;
    on_seg_created(commands, root, 0);
    // start at 1 to account for root
    // * 2 - 1 because we are inserting joints along with segments
    for ix in (1..segment_count * 2 - 1) {
        let direction = dir_from_ix(ix);

        let Some(direction) = direction else {
            continue;
        };

        let extend_result = match direction {
            Direction::Left => parent.extend_left(commands)?,
            Direction::Right => parent.extend_right(commands)?,
        };
        if let Some(new_joint) = extend_result.created_joint {
            joints.entry(direction).or_default().push(new_joint.entity);
            on_joint_created(commands, new_joint.entity, new_joint.mesh_joint);
        }
        if let Some(new_segment) = extend_result.created_segment {
            segments.entry(direction).or_default().push(new_segment);
            on_seg_created(commands, new_segment, ix);
        }
    }

    #[derive(Debug, Copy, Clone)]
    enum NeedsTransform {
        Segment(Entity),
        Joint(Entity),
    }

    impl NeedsTransform {
        fn entity(&self) -> Entity {
            match *self {
                Self::Segment(entity) => entity,
                Self::Joint(entity) => entity,
            }
        }
    }

    #[derive(Debug, Copy, Clone)]
    enum DirectionWrapped<T> {
        Left(T),
        Right(T),
    }

    impl<T> DirectionWrapped<T> {
        fn new(direction: Direction, item: T) -> Self {
            match direction {
                Direction::Left => Self::Left(item),
                Direction::Right => Self::Right(item),
            }
        }
        fn direction(&self) -> Direction {
            match self {
                Self::Left(_) => Direction::Left,
                Self::Right(_) => Direction::Right,
            }
        }
        fn item(&self) -> &T {
            match self {
                Self::Left(item) => item,
                Self::Right(item) => item,
            }
        }
        fn tuple(&self) -> (Direction, &T) {
            match self {
                Self::Left(item) => (Direction::Left, item),
                Self::Right(item) => (Direction::Right, item),
            }
        }
    }

    impl<T1> DirectionWrapped<T1>
    where
        T1: Copy,
    {
        fn from_map<T2>(
            map: HashMap<Direction, Vec<T2>>,
            constructor: fn(T2) -> T1,
        ) -> Vec<(usize, Self)> {
            map.into_iter()
                .flat_map(|(direction, items)| {
                    items
                        .into_iter()
                        .enumerate()
                        .map(move |(ix, item)| (ix, Self::new(direction, constructor(item))))
                })
                .collect()
        }
    }

    let needs_transforms = []
        .into_iter()
        .chain(DirectionWrapped::from_map(joints, NeedsTransform::Joint))
        .chain(DirectionWrapped::from_map(
            segments,
            NeedsTransform::Segment,
        ))
        .collect::<Vec<_>>();

    for (ix, need_transform) in needs_transforms {
        let (direction, need_transform) = need_transform.tuple();
        let dir = direction.to_sign() as f32 * Vec3::X;
        // +1 since root isn't included
        // let ix = (ix + 1) as f32;
        // TODO: apparently all this effort isn't needed
        //  because we are using local transforms that already account for the previous parent transform
        let translation = match need_transform {
            NeedsTransform::Segment(_) => dir * segment_width - dir * PATTY_JOINT_WIDTH,
            NeedsTransform::Joint(_) => dir * segment_width - dir * PATTY_JOINT_WIDTH,
        };
        commands
            .entity(need_transform.entity())
            .insert(Transform::from_translation(translation));
    }

    Ok(patty_structure)
}
