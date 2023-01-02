#![cfg_attr(
  all(not(debug_assertions), target_os = "windows"),
  windows_subsystem = "windows"
)]

use std::fs;                    // add imports for filesystem access
use std::time::SystemTime;      // and times


#[tauri::command]
fn modified_time(file_path: String) -> Result<SystemTime, String> {
    modified_time_of(file_path).map_err(|err| err.to_string())        // separate function to change either error to String
}

fn modified_time_of(file_path: String) -> Result<SystemTime, std::io::Error> {
  let meta = fs::metadata(&file_path)?;
  meta.modified()

}

fn main() {
  tauri::Builder::default()
    .invoke_handler(tauri::generate_handler![modified_time])        // Add handler for the function. Function names should be snake case.
    .run(tauri::generate_context!())
    .expect("error while running tauri application");
}
