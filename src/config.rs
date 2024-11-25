use bevy::prelude::Resource;
use serde::{Deserialize, Serialize};
use std::{fs, io};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ConfigError {
    #[error("Configuration file not found")]
    FileNotFound(#[from] io::Error),
    #[error("Configuration file failed to parse")]
    ParseError(#[from] toml::de::Error),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct WindowConfig {
    pub opacity: Option<f32>,
}

#[derive(Serialize, Deserialize, Debug, Resource)]
pub struct TerminalConfig {
    pub window: WindowConfig,
}

impl TerminalConfig {
    pub fn read() -> Result<Self, ConfigError> {
        let data_dir = dirs::config_dir()
            .unwrap_or(dirs::home_dir().unwrap().join(".config"))
            .join("milkshake-terminal");
        if !std::fs::exists(&data_dir)? {
            std::fs::create_dir_all(&data_dir)?;
        }

        let path = data_dir.join("config.toml");

        if !std::fs::exists(&path)? {
            std::fs::write(&path, "[window]\nopacity = 1.0")?;
        }

        let content = fs::read_to_string(path)?;

        Ok(toml::from_str(&content)?)
    }
}
