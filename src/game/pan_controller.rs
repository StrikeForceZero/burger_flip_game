use crate::game::pan;
use crate::game::pan::Pan;
use crate::AppSet;
use avian2d::prelude::{AngularVelocity, PhysicsSchedule, PhysicsStepSet};
use bevy::ecs::query::QueryData;
use bevy::prelude::*;
use internal_bevy_auto_plugin_macros::{auto_plugin, auto_register_type};
use num_traits::real::Real;
use std::f32::consts::PI;

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
    app.add_systems(
        PhysicsSchedule,
        (apply_input).chain().in_set(PhysicsStepSet::Last),
    );
}

#[derive(QueryData)]
#[query_data(mutable)]
struct ApplyInputQueryData<'w> {
    pan_controller: Ref<'w, PanController>,
    transform: Mut<'w, Transform>,
    angular_velocity: Mut<'w, AngularVelocity>,
}

fn apply_input(mut movement_query: Query<ApplyInputQueryData, With<Pan>>) {
    const EPSILON: f32 = PI / 180.0 / 10.0; // 1/10 deg
    for mut item in &mut movement_query {
        let current_rotation = item.transform.rotation.to_euler(EulerRot::ZYX).0;
        let clamped_rotation = current_rotation.clamp(pan::PAN_MIN_RADIANS, pan::PAN_MAX_RADIANS);
        let at_or_over_max = clamped_rotation >= pan::PAN_MAX_RADIANS - EPSILON;
        let at_or_under_min = clamped_rotation <= pan::PAN_MIN_RADIANS + EPSILON;
        let requested_rotation = item.pan_controller.intent;

        let velocity = if at_or_over_max && requested_rotation.is_some()
            || at_or_under_min && requested_rotation.is_none()
        {
            0.0
        } else {
            requested_rotation.unwrap_or(-item.pan_controller.rotation_speed / 2.0)
        };
        item.transform.rotation = Quat::from_rotation_z(clamped_rotation);
        item.angular_velocity.0 = velocity;
    }
}
