# xtra-ui
This is the Elm-based frontend for Tracr (formerly Xtra-UI).

This repository only needs to be used if you are planning to make frontend changes to the interface.

A pre-compiled version exists within this repository (elm.js), and is also mirrored in the xtra-backend repository under ```/www```. Since the js only needs to be served statically, cloning this repository is not needed to simple run the project.

Notes for running the backend (/hosting) server exist within that repository.

If you do need to build the project, you can use the following command:

```
elm make src/Main.elm --optimize --output elm.js
```

If you have any questions, please feel free to reach out to me: jack@jackbarnes.dev
