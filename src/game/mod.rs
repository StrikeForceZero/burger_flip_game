use bevy::prelude::*;
use internal_bevy_auto_plugin_macros::auto_plugin;

pub mod bun;
pub mod pan;
pub mod pan_controller;
pub mod patty;

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_plugins(pan::plugin);
    app.add_plugins(bun::plugin);
    app.add_plugins(patty::plugin);
    app.add_plugins(pan_controller::plugin);
}
