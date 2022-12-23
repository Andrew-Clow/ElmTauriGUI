# Elm Tauri Example

This uses Tauri.app via lobanov/elm-taskport to connect your elm app to the file system etc.

## How to set everything up

1. Follow https://tauri.app/v1/guides/getting-started/setup/
2. elm init
3. create intelliJ project from existing sources
4. Install elm modules:
     - elm install mdgriffith/elm-ui
     - elm install lobanov/elm-taskport
     - elm install elm/json
5. Copy taskport.min.js into publicUI 
6. Make a viable Main.elm
7. Create index.html from the Elm guide with Taskport entries for Tauri stuff, like

     <script src="./taskport.min.js"></script>
     <script>
        TaskPort.install(); // can pass a settings object as a parameter, see https://elm.dmy.fr/packages/lobanov/elm-taskport/latest/
        TaskPort.register("readTextFile", (args) => {return window.__TAURI__.fs.readTextFile(args)});
        TaskPort.register("open", (args) => {return window.__TAURI__.dialog.open(args)});
        TaskPort.register("ask", (args) => {return window.__TAURI__.dialog.ask(args)});
     </script>

   Note that if you get an interop error like TaskPort Not Installed, you probably just have a typo in the javascript above. 
   If it says Not Found, it's probably because you omitted that line altogether or commented it out and didn't reinstate it.
8. cargo tauri init
     - dev server ../publicUI
     - dev command `elm make src/Main.elm --output=publicUI/main.js`
     - build command `elm make src/Main.elm --output=publicUI/main.js --optimize`
     - edit the borked tauri.conf.json to include the = sign instead of UUencoded nonsense in what I just typed. 
9. Add FS and Path and Dialog on allowlist in tauri.conf.json.
     * Note the `["$APPCONFIG/*","$APPCONFIG", ... ]` to workaround the lack of access to root directory malfeature in tauri.
10. Add `"withGlobalTauri": true` into the build section of tauri.conf.json    
11. Add the [elm guide](https://guide.elm-lang.org/interop/) index.html file in publicUI
12. `cargo tauri dev` to check it's working
13. Norton AV errors with build-script-build.exe. Restore it.
    * Exclude the whole folder from further interruption:
    * https://support.norton.com/sp/en/us/home/current/solutions/v3672136

## Handy hot reloading of everything in sight:

* npx elm-watch hot
* cargo tauri dev

## TODO:

* Maybe provide three `toMsg` functions, one for interop failure, one for tauri failure and one for tauri success.
  This might be good particularly for the Dialogs because the bad path is cluttering up the Msg type.
* Provide a Task interface as well as a Cmd interface, perhaps with confirm stuff using `andThen`.  