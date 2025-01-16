use crate::game::bun::SpawnBun;
use crate::game::patty::SpawnPatty;
use crate::screens::Screen;
use avian2d::prelude::Collision;
use bevy::prelude::*;
use internal_bevy_auto_plugin_macros::{
    auto_add_event, auto_init_resource, auto_plugin, auto_register_type,
};

pub mod bun;
pub mod pan;
pub mod pan_controller;
pub mod patty;

#[auto_register_type]
#[auto_add_event]
#[derive(Event, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub struct GameOver;

#[auto_register_type]
#[auto_add_event]
#[derive(Event, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub struct PattyOutOfBounds;

#[auto_register_type]
#[auto_add_event]
#[derive(Event, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
pub struct PattyLanded;

#[auto_register_type]
#[auto_init_resource]
#[derive(Resource, Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Reflect)]
#[reflect(Resource)]
pub struct Score(pub u32);

fn on_patty_out_of_bounds(
    mut commands: Commands,
    mut collision_event_reader: EventReader<Collision>,
    mut events: EventReader<PattyOutOfBounds>,
    bun_sensor: Single<Entity, With<bun::BunAreaSensor>>,
    patties: Query<Entity, With<patty::Patty>>,
) {
    for _ in events.read() {
        for Collision(contacts) in collision_event_reader.read() {
            if (*bun_sensor == contacts.entity1 && patties.contains(contacts.entity2)
                || (*bun_sensor == contacts.entity2 && patties.contains(contacts.entity1)))
            {
                commands.send_event(GameOver);
                return;
            }
        }
    }
}

fn on_game_over(
    mut commands: Commands,
    mut events: EventReader<GameOver>,
    bun: Single<Entity, With<bun::Bun>>,
    patties: Query<Entity, With<patty::Patty>>,
    mut score: ResMut<Score>,
) {
    for _ in events.read() {
        log::info!("Game Over {}", score.0);
        score.0 = 0;
        for patty in patties.iter() {
            commands.entity(patty).try_despawn_recursive();
        }
        commands.entity(*bun).try_despawn_recursive();
        commands.queue(SpawnBun {
            pos: Vec2::X * 300.0,
        });
        commands.queue(SpawnPatty {
            pos: Vec2::Y * 100.0 + Vec2::X * -20.0,
            scale: Vec2::splat(1.5),
        });
    }
}

fn on_landed(mut events: EventReader<PattyLanded>, mut score: ResMut<Score>) {
    for _ in events.read() {
        score.0 += 1;
        log::info!("Score {}", score.0);
    }
}

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_plugins(pan::plugin);
    app.add_plugins(bun::plugin);
    app.add_plugins(patty::plugin);
    app.add_plugins(pan_controller::plugin);

    app.add_systems(
        FixedUpdate,
        (on_patty_out_of_bounds, on_landed, on_game_over)
            .chain()
            .run_if(Screen::run_if_is_gameplay),
    );
}
