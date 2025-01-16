//! Spawn the main level.

use bevy::{ecs::world::Command, prelude::*};
use std::f32::consts::FRAC_PI_2;

use crate::game::bun::SpawnBun;
use crate::game::pan::SpawnPan;
use crate::game::patty::SpawnPatty;

pub(super) fn plugin(_app: &mut App) {
    // No setup required for this plugin.
    // It's still good to have a function here so that we can add some setup
    // later if needed.
}

/// A [`Command`] to spawn the level.
/// Functions that accept only `&mut World` as their parameter implement [`Command`].
/// We use this style when a command requires no configuration.
pub fn spawn_level(world: &mut World) {
    SpawnPan {
        pos: Vec2::ZERO - Vec2::X * 200.0,
        rotation_speed: FRAC_PI_2 * 2.0,
    }
    .apply(world);
    SpawnPatty {
        pos: Vec2::Y * 100.0 + Vec2::X * -20.0,
        scale: Vec2::splat(1.5),
    }
    .apply(world);
    SpawnBun {
        pos: Vec2::X * 300.0,
    }
    .apply(world);
}
