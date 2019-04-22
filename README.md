## Elm-update

The `elm-update` command allows to list the updatable dependencies listed in an `elm.json` file.
Future releases will allow optional auto update of the file as well.

### Usage

```bash
elm-update
```

Will look for a `./elm.json` file by default.

You can also provide a custom path:

```bash
elm-update --project ./custom/path/to/elm.json
```

Get help with:

```bash
elm-update --help
```
