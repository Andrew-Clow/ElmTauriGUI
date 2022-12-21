# Elm Tauri Example

## How to set everything up

0. Follow https://tauri.app/v1/guides/getting-started/setup/
1. elm init
2. create intelliJ project from existing sources
3. Install elm modules:
     - elm install mdgriffith/elm-ui
     - elm install lobanov/elm-taskport
     - elm install elm/json
3. Copy taskport.min.js into publicUI 
4. Make a viable Main.elm
4. Create index.html from the Elm guide with Taskport entries for Tauri stuff, like

   <script src="./taskport.min.js"></script>
   <script>
   TaskPort.install(); // can pass a settings object as a parameter, see https://elm.dmy.fr/packages/lobanov/elm-taskport/latest/
   TaskPort.register("readTextFile", (args) => {return window.__TAURI__.fs.readTextFile(args)});
   TaskPort.register("open", (args) => {return window.__TAURI__.dialog.open(args)});
   TaskPort.register("ask", (args) => {return window.__TAURI__.dialog.ask(args)});
   </script>

5. cargo tauri init
     - dev server ../publicUI
     - dev command `elm make src/Main.elm --output=publicUI/main.js`
     - build command `elm make src/Main.elm --output=publicUI/main.js --optimize`
     - edit the borked tauri.conf.json to include the = sign instead of UUencoded nonsense in what I just typed. 
6. Add FS and Path and Dialog on allowlist in tauri.conf.json.
     * Note the `["$APPCONFIG/*","$APPCONFIG", ... ]` to workaround the lack of access to root directory malfeature in tauri.
6. Add `"withGlobalTauri": true` into the build section of tauri.conf.json    
7. Add the [elm guide](https://guide.elm-lang.org/interop/) index.html file in publicUI
8. `cargo tauri dev` to check it's working
9. Norton AV errors case 74725700 with build-script-build.exe. Restore it.
    * Exclude the whole folder from further interruption:
    * https://support.norton.com/sp/en/us/home/current/solutions/v3672136

## TODO:

* Maybe provide two `toMsg` functions, one for success and one for failure.
  This might be good particularly for the Dialogs because the bad path is cluttering up the Msg type.
* Provide a Task interface as well as a Cmd interface, perhaps with confirm stuff using `andThen`.  