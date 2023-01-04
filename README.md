# Elm Tauri Example

Connects elm to the file system etc using Tauri.app via lobanov/elm-taskport. Mini HowTo below.

There's an example app with LOTS of buttons in Main.elm with persistent Main/Congfig.elm.

![screenshot showing many buttons like "Open Dialog" "Copy File" etc](screenshot.png?raw=true "Screenshot")

If you're cloning it to edit, you'll certainly want to edit tauri.conf.json from what I put:

    "package": {
        "productName": "ElmTauriGUI",
    "tauri": {
        ...
        ...
        ...
        "identifier": "Andrew-Clow.ElmTauriGUI",


# How to set everything up for Elm talking to Tauri via TaskPort:

1. Follow 
   - https://tauri.app/v1/guides/getting-started/prerequisites
   - https://tauri.app/v1/guides/getting-started/setup/
2. elm init
3. (create IDE project from existing sources)
4. Install elm modules:
     - elm install mdgriffith/elm-ui
     - elm install lobanov/elm-taskport
     - elm install elm/json
5. Copy `taskport.min.js` into publicUI. See https://package.elm-lang.org/packages/lobanov/elm-taskport/latest/
6. Make a viable `Main.elm`
7. Create `index.html` in `putblicUI` from the [Elm guide](https://guide.elm-lang.org/interop/) with Taskport entries for Tauri stuff as per the TaskPort guide and Tauri API, like

       <script src="./taskport.min.js"></script>
       <script>
          TaskPort.install(); // can pass a settings object as a parameter, see lobanov/elm-taskport/
          TaskPort.register("readTextFile", (args) => {return window.__TAURI__.fs.readTextFile(args)});
          TaskPort.register("open", (args) => {return window.__TAURI__.dialog.open(args)});
          TaskPort.register("ask", (args) => {return window.__TAURI__.dialog.ask(args)});
       </script>

   Note that if you get an interop error like `Not Installed`, you probably just have a typo in the javascript above. 
   If it says `Not Found`, it's probably because you omitted that line altogether or commented it out and didn't reinstate it.
8. `cargo tauri init`
     - dev server `../publicUI`
     - dev command `elm make src/Main.elm --output=publicUI/main.js`
     - build command `elm make src/Main.elm --output=publicUI/main.js --optimize`
     - On Windows you may need to edit the borked `tauri.conf.json` to include the = sign instead of UUencoded nonsense in what you just typed. 
9. Add `FS` and `Path` and `Dialog` stuff on `allowlist` in `tauri.conf.json`. See the one here for examples.
     * Note the `["$APPCONFIG/*","$APPCONFIG", ... ]` to workaround the lack of access to root directory malfeature in Tauri.
10. Add `"withGlobalTauri": true` into the build section of `tauri.conf.json` to enable all that `window.__TAURI__.` stuff.    
11. `cargo tauri dev` to check it's working.
12. The stuff in Tauri.Modified requires edits to your src-tauri/main.rs to give the rust code backing that up as it's not part of tauri.
13. Antivirus: Norton AV panics about build-script-build.exe. Restore it.
    * Exclude the whole folder from further interruption:
    * https://support.norton.com/sp/en/us/home/current/solutions/v3672136

## Handy hot reloading of everything in sight:

* npx elm-watch hot
* cargo tauri dev

# Todo

I haven't tested _all_ of the functions! Sorry. I've tested most of them, and more than listed here.