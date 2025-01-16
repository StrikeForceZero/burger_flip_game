use crate::game::patty::MeshSegmentRoot;
use crate::screens::Screen;
use avian2d::prelude::*;
use bevy::prelude::*;
use internal_bevy_auto_plugin_macros::{auto_init_resource, auto_plugin, auto_register_type};

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "BunAreaSensor"))]
#[require(Transform)]
#[require(Visibility)]
pub struct BunAreaSensor;

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "Bun"))]
#[require(RigidBody(|| RigidBody::Kinematic))]
#[require(AngularVelocity)]
#[require(Transform)]
#[require(Visibility)]
pub struct Bun;

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct BunMesh(Option<Mesh2d>);

impl BunMesh {
    pub fn clone_handle(&self) -> Option<Mesh2d> {
        self.0.clone()
    }
}

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Reflect)]
#[reflect(Resource)]
pub struct BunMaterial(Option<MeshMaterial2d<ColorMaterial>>);

impl BunMaterial {
    pub fn clone_handle(&self) -> Option<MeshMaterial2d<ColorMaterial>> {
        self.0.clone()
    }
}

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_systems(PreStartup, (init_mesh, init_material).chain());
}

const BUN_WIDTH: f32 = 150.0;
const BUN_HEIGHT: f32 = 25.0;

#[derive(Debug)]
pub struct SpawnBun {
    pub pos: Vec2,
}

impl Command for SpawnBun {
    fn apply(self, world: &mut World) {
        let _ = world.run_system_cached_with(spawn_bun, self);
    }
}

fn spawn_bun(
    In(config): In<SpawnBun>,
    mut commands: Commands,
    bun_mesh: Res<BunMesh>,
    bun_material: Res<BunMaterial>,
) {
    commands
        .spawn((
            Bun,
            NoAutoCenterOfMass,
            StateScoped(Screen::Gameplay),
            Transform::from_translation(config.pos.extend(0.0)),
            Collider::rectangle(BUN_WIDTH, BUN_HEIGHT),
            bun_mesh.0.clone().expect("Bun mesh not initialized"),
            bun_material
                .0
                .clone()
                .expect("Bun material not initialized"),
        ))
        .with_children(|parent| {
            parent.spawn((
                BunAreaSensor,
                Transform::from_translation(Vec3::Y * 500.0),
                InheritedVisibility::default(),
                Sensor,
                Collider::rectangle(BUN_WIDTH, 1000.0),
            ));
        });
}

fn init_mesh(mut bun_mesh: ResMut<BunMesh>, mut meshes: ResMut<Assets<Mesh>>) {
    bun_mesh.0 = Some(Mesh2d(meshes.add(Rectangle::new(BUN_WIDTH, BUN_HEIGHT))));
}

fn init_material(
    mut bun_material: ResMut<BunMaterial>,
    mut materials: ResMut<Assets<ColorMaterial>>,
) {
    bun_material.0 = Some(MeshMaterial2d(materials.add(ColorMaterial::from_color(
        bevy::color::palettes::css::BURLYWOOD,
    ))));
}
