use bevy::prelude::*;
use internal_bevy_auto_plugin_macros::auto_plugin;

pub mod pan;
pub mod patty;
pub mod pan_controller;

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_plugins(pan::plugin);
    app.add_plugins(patty::plugin);
    app.add_plugins(pan_controller::plugin);
}
