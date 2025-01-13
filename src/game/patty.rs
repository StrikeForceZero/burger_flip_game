use crate::screens::Screen;
use avian2d::math::Scalar;
use avian2d::prelude::*;
use bevy::asset::RenderAssetUsages;
use bevy::prelude::*;
use bevy::render::mesh::{Indices, PrimitiveTopology};
use internal_bevy_auto_plugin_macros::{auto_init_resource, auto_plugin, auto_register_type};
use smart_default::SmartDefault;
use std::f32::consts::PI;
use itertools::Itertools;

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

const PATTY_PARTS: f32 = 6.0;
const PATTY_WIDTH: f32 = 100.0;
const PATTY_PART_WIDTH: f32 = PATTY_WIDTH / PATTY_PARTS;
const PATTY_HEIGHT: f32 = 20.0;

fn spawn_patty(In(config): In<SpawnPatty>, mut commands: Commands) {
    let spawn_patty = |ix: usize| -> Entity {
        let offset = Vec2::X * (ix as f32 - PATTY_PARTS / 2.0) * PATTY_PART_WIDTH;
        let translation = (config.pos + offset * config.scale).extend(0.0);
        let transform_scale = config.scale.extend(1.0);
        commands
            .spawn((
                Patty,
                NoAutoCenterOfMass,
                StateScoped(Screen::Gameplay),
                Transform::from_translation(translation).with_scale(transform_scale),
            ))
            .id()
    };
    let patty_parts = (0..(PATTY_PARTS as usize)).map(spawn_patty).collect_vec();
    for (left, right) in patty_parts.into_iter().tuple_windows() {
        let half_width = PATTY_PART_WIDTH * config.scale / 2.0;
        commands.spawn(
            RevoluteJoint::new(left, right)
                .with_local_anchor_1(Vec2::X * half_width)
                .with_local_anchor_2(Vec2::NEG_X * half_width)
                .with_compliance(0.0000001),
        );
    }
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

    commands.entity(trigger.entity()).insert((
        mesh,
        material,
        Collider::rectangle(PATTY_PART_WIDTH, PATTY_HEIGHT),
    ));
}

fn init_mesh(mut pan_mesh: ResMut<PattyMesh>, mut meshes: ResMut<Assets<Mesh>>) {
    if pan_mesh.0.is_some() {
        return;
    }
    let handle = meshes.add(Rectangle::new(PATTY_PART_WIDTH, PATTY_HEIGHT));
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
