use crate::game::pan_controller::PanController;
use crate::game::patty::Patty;
use crate::screens::Screen;
use crate::AppSet;
use avian2d::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use internal_bevy_auto_plugin_macros::{auto_init_resource, auto_plugin, auto_register_type};
use std::f32::consts::{FRAC_PI_3, FRAC_PI_4, FRAC_PI_8};

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "PanAreaSensor"))]
#[require(Transform)]
#[require(Visibility)]
pub struct PanAreaSensor;

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct HasPattyInArea(bool);

impl HasPattyInArea {
    pub fn in_area(&self) -> bool {
        self.0
    }
}

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "Pan"))]
#[require(RigidBody(|| RigidBody::Kinematic))]
#[require(AngularVelocity)]
#[require(Transform)]
#[require(Visibility)]
pub struct Pan;

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct PanMesh(Option<Mesh2d>);

impl PanMesh {
    pub fn clone_handle(&self) -> Option<Mesh2d> {
        self.0.clone()
    }
}

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct PanMaterial(Option<MeshMaterial2d<ColorMaterial>>);

impl PanMaterial {
    pub fn clone_handle(&self) -> Option<MeshMaterial2d<ColorMaterial>> {
        self.0.clone()
    }
}

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_systems(PreStartup, (init_mesh, init_material).chain());
    app.add_observer(on_pan_add);
    app.add_systems(
        FixedUpdate,
        (handle_mouse_input).chain().in_set(AppSet::RecordInput),
    );
    app.add_systems(
        FixedUpdate,
        (patties_in_area).chain().in_set(AppSet::Update),
    );
}

fn patties_in_area(
    mut collision_event_reader: EventReader<Collision>,
    mut has_patty_in_area: ResMut<HasPattyInArea>,
    sensor: Single<Entity, With<PanAreaSensor>>,
    patties: Query<&RigidBody, With<Patty>>,
) {
    if patties.iter().all(RigidBody::is_static) {
        return;
    }
    let mut new_has_patty_in_area = false;
    for Collision(contacts) in collision_event_reader.read() {
        if (*sensor == contacts.entity1 && patties.contains(contacts.entity2)
            || (*sensor == contacts.entity2 && patties.contains(contacts.entity1)))
        {
            new_has_patty_in_area = true;
        }
    }

    has_patty_in_area.0 = new_has_patty_in_area;
}

pub const PAN_MIN_RADIANS: f32 = -FRAC_PI_8;
pub const PAN_MAX_RADIANS: f32 = 0.0;

fn handle_mouse_input(
    input: Res<ButtonInput<MouseButton>>,
    mut controller_query: Query<&mut PanController, With<Pan>>,
) {
    for mut controller in &mut controller_query.iter_mut() {
        controller.intent = if input.pressed(MouseButton::Left) {
            Some(controller.rotation_speed)
        } else {
            None
        };
    }
}

#[derive(Debug)]
pub struct SpawnPan {
    pub pos: Vec2,
    pub rotation_speed: f32,
}

impl Command for SpawnPan {
    fn apply(self, world: &mut World) {
        let _ = world.run_system_cached_with(spawn_pan, self);
    }
}

fn spawn_pan(In(config): In<SpawnPan>, mut commands: Commands) {
    commands.spawn((
        Pan,
        NoAutoCenterOfMass,
        StateScoped(Screen::Gameplay),
        Transform::from_translation(config.pos.extend(0.0))
            .with_scale(Vec2::splat(20.0).extend(1.0))
            .with_rotation(Quat::from_axis_angle(Vec3::Z, PAN_MIN_RADIANS)),
        PanController {
            rotation_speed: config.rotation_speed,
            ..default()
        },
    ));
    commands.spawn((
        PanAreaSensor,
        Sensor,
        Transform::from_translation(config.pos.extend(0.0) + Vec3::X * 130.0),
        StateScoped(Screen::Gameplay),
        Collider::rectangle(300.0, 400.0),
    ));
}

fn on_pan_add(
    trigger: Trigger<OnAdd, Pan>,
    mut commands: Commands,
    pan_mesh: Res<PanMesh>,
    pan_material: Res<PanMaterial>,
) {
    let mesh = pan_mesh.clone_handle().expect("Pan mesh not initialized");
    let material = pan_material
        .clone_handle()
        .expect("Pan material not initialized");

    commands.entity(trigger.entity()).insert((
        mesh,
        material,
        ColliderConstructor::ConvexHull {
            points: skillet_vertices().map(Vec2::from_array).to_vec(),
        },
    ));
}

fn init_mesh(mut pan_mesh: ResMut<PanMesh>, mut meshes: ResMut<Assets<Mesh>>) {
    if pan_mesh.0.is_some() {
        return;
    }
    let handle = meshes.add(create_skillet_mesh());
    pan_mesh.0 = Some(Mesh2d(handle));
}

fn init_material(
    mut pan_material: ResMut<PanMaterial>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    if pan_material.0.is_some() {
        return;
    }
    let handle = materials.add(ColorMaterial::from_color(
        bevy::color::palettes::tailwind::GRAY_900,
    ));
    pan_material.0 = Some(MeshMaterial2d(handle));
}

const fn skillet_vertices() -> [[f32; 2]; 7] {
    [
        [13.0, 0.0],  // v0 (pan top right)
        [4.0, 0.0],   // v1 (pan top left/start of handle)
        [0.0, 0.0],   // v2 (handle top left - origin)
        [0.0, -1.0],  // v3 (handle bottom left)
        [4.0, -1.0],  // v4 (handle bottom right)
        [5.0, -3.0],  // v5 (pan bottom left)
        [12.5, -3.0], // v6 (pan bottom right)
    ]
}

fn create_skillet_mesh() -> Mesh {
    const fn with_zero_z(v: [f32; 2]) -> [f32; 3] {
        [v[0], v[1], 0.0]
    }
    // Define the vertex positions
    let positions = skillet_vertices().map(with_zero_z).to_vec();

    // Define vertex normals (required for mesh, but can be 0 since it's 2D)
    let normals = vec![[0.0, 0.0, 1.0]; 7];

    // Define UV coordinates (not needed but required for completeness)
    let uvs = vec![
        [0.0, 1.0],  // v0
        [1.0, 1.0],  // v1
        [1.0, 0.5],  // v2
        [1.0, 0.0],  // v3
        [0.5, 0.0],  // v4
        [0.5, -0.5], // v5
        [0.0, -0.5], // v6
    ];

    // Define the indices for the triangles
    let indices = Indices::U32(vec![
        0, 6, 5, /* */
        0, 5, 4, /* */
        0, 4, 1, /* */
        1, 2, 3, /* */
        1, 3, 4, /* */
    ]);

    // Create the mesh
    let mut mesh = Mesh::new(
        PrimitiveTopology::TriangleList,
        RenderAssetUsages::MAIN_WORLD | RenderAssetUsages::RENDER_WORLD,
    );
    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
    mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
    mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, uvs);
    mesh.insert_indices(indices);

    mesh
}
