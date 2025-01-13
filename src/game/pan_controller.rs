use crate::game::pan;
use crate::game::pan::Pan;
use crate::AppSet;
use avian2d::prelude::AngularVelocity;
use bevy::ecs::query::QueryData;
use bevy::prelude::*;
use num_traits::real::Real;
use internal_bevy_auto_plugin_macros::{auto_plugin, auto_register_type};

#[auto_register_type]
#[derive(Component, Debug, Default, Clone, Copy, PartialEq, Reflect)]
#[reflect(Component)]
#[require(Name(|| "PanController"))]
pub struct PanController {
    pub intent: Option<f32>,
    pub rotation_speed: f32,
}

#[auto_plugin(app=app)]
pub(crate) fn plugin(app: &mut App) {
    app.add_systems(FixedUpdate, (apply_input).chain().in_set(AppSet::Update));
}

#[derive(QueryData)]
#[query_data(mutable)]
struct ApplyInputQueryData<'w> {
    pan_controller: Ref<'w, PanController>,
    transform: Mut<'w, Transform>,
    angular_velocity: Mut<'w, AngularVelocity>,
}

fn apply_input(mut movement_query: Query<ApplyInputQueryData, With<Pan>>) {
    for mut item in &mut movement_query {
        let at_or_over_max = item.transform.rotation.z >= pan::PAN_MAX_RADIANS;
        let at_or_under_min = item.transform.rotation.z <= pan::PAN_MIN_RADIANS;
        let requested_rotation = item.pan_controller.intent;
        let velocity = if at_or_over_max && requested_rotation.map(Real::is_sign_positive) == Some(true)
            || at_or_under_min && requested_rotation.map(Real::is_sign_negative) == Some(true)
        {
            0.0
        } else {
            requested_rotation.unwrap_or(-item.pan_controller.rotation_speed)
        };
        let clamped_rotation = item
            .transform
            .rotation
            .z
            .clamp(pan::PAN_MIN_RADIANS, pan::PAN_MAX_RADIANS);
        item.transform.rotation.z = clamped_rotation;
        item.angular_velocity.0 = velocity;
    }
}
