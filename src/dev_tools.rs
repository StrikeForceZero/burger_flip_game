//! Development tools for the game. This plugin is only enabled in dev builds.

use crate::screens::Screen;
#[cfg(feature = "show_colliders")]
use avian2d::prelude::PhysicsDebugPlugin;
use bevy::{
    dev_tools::{
        states::log_transitions,
        ui_debug_overlay::{DebugUiPlugin, UiDebugOptions},
    },
    input::common_conditions::input_just_pressed,
    prelude::*,
};
#[cfg(feature = "inspector")]
use bevy_inspector_egui::quick::WorldInspectorPlugin;

pub(super) fn plugin(app: &mut App) {
    #[cfg(feature = "inspector")]
    app.add_plugins(WorldInspectorPlugin::default());
    // Log `Screen` state transitions.
    app.add_systems(Update, log_transitions::<Screen>);
    #[cfg(feature = "show_colliders")]
    // Visualize Colliders
    app.add_plugins(PhysicsDebugPlugin::default());

    // Toggle the debug overlay for UI.
    app.add_plugins(DebugUiPlugin);
    app.add_systems(
        Update,
        toggle_debug_ui.run_if(input_just_pressed(TOGGLE_KEY)),
    );
}

const TOGGLE_KEY: KeyCode = KeyCode::Backquote;

fn toggle_debug_ui(mut options: ResMut<UiDebugOptions>) {
    options.toggle();
}
