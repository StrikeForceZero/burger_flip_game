use crate::screens::Screen;
use avian2d::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use internal_bevy_auto_plugin_macros::{auto_init_resource, auto_plugin, auto_register_type};
use smart_default::SmartDefault;

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Component)]
#[require(Name(|| "Patty"))]
#[require(RigidBody(|| RigidBody::Dynamic))]
#[require(AngularVelocity)]
#[require(LinearVelocity)]
#[require(Transform)]
#[require(Visibility)]
pub struct Patty;

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

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_systems(PreStartup, (init_mesh, init_material).chain());
    app.add_observer(on_patty_add);
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

fn spawn_patty(In(config): In<SpawnPatty>, mut commands: Commands) {
    commands.spawn((
        Patty,
        StateScoped(Screen::Gameplay),
        Transform::from_translation(config.pos.extend(0.0)).with_scale(config.scale.extend(1.0)),
    ));
}

fn on_patty_add(
    trigger: Trigger<OnAdd, Patty>,
    mut commands: Commands,
    pan_mesh: Res<PattyMesh>,
    pan_material: Res<PattyMaterial>,
) {
    let mesh = pan_mesh.clone_handle().expect("Patty mesh not initialized");
    let material = pan_material
        .clone_handle()
        .expect("Patty material not initialized");

    commands
        .entity(trigger.entity())
        .insert((mesh, material, Collider::rectangle(100.0, 20.0)));
}

fn init_mesh(mut pan_mesh: ResMut<PattyMesh>, mut meshes: ResMut<Assets<Mesh>>) {
    if pan_mesh.0.is_some() {
        return;
    }
    let handle = meshes.add(Rectangle::new(100.0, 20.0));
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
