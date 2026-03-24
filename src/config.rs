use directories::ProjectDirs;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Config {
    #[serde(default = "default_bloom")]
    pub bloom: BloomConfig,
    #[serde(default = "default_transparency")]
    pub transparency: TransparencyConfig,
    #[serde(default = "default_explosion")]
    pub explosion: ExplosionConfig,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct BloomConfig {
    pub enabled: bool,
    pub intensity: f32,
    pub threshold: f32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct TransparencyConfig {
    pub enabled: bool,
    pub opacity: f32,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ExplosionConfig {
    pub enabled: bool,
}

fn default_bloom() -> BloomConfig {
    BloomConfig {
        enabled: true,
        intensity: 0.4,
        threshold: 0.3,
    }
}

fn default_transparency() -> TransparencyConfig {
    TransparencyConfig {
        enabled: true,
        opacity: 0.85,
    }
}

fn default_explosion() -> ExplosionConfig {
    ExplosionConfig { enabled: true }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            bloom: default_bloom(),
            transparency: default_transparency(),
            explosion: default_explosion(),
        }
    }
}

impl Config {
    pub fn load() -> Self {
        let config_path = Self::get_path();
        if let Ok(content) = fs::read_to_string(&config_path)
            && let Ok(config) = toml::from_str(&content)
        {
            return config;
        }

        let default = Self::default();
        fs::create_dir_all(config_path.parent().unwrap())
            .expect("Failed to create config directory");
        fs::write(config_path, toml::to_string(&default).unwrap())
            .expect("Failed to write config file");
        default
    }

    fn get_path() -> PathBuf {
        ProjectDirs::from("com", "milkshake-terminal", "milkshake-terminal")
            .expect("Failed to find config directory")
            .config_dir()
            .join("config.toml")
    }
}
