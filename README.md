imgup
=====

Upload an image anonymously to imgur

### Screenshots
Upload your most recent screenshot anonymously with the `--screenshot` flag

```
imgup --screenshot |pbcopy
```

![screenshot example](screenshot_example.png)

As of now this requires `Screen Shot*` as the naming convention _AND_ a 24 hour clock.

### From Your Filesystem
```
imgup ~/Desktop/pepe-happy.png
```

### Reuploaded to Imgur
```
imgup -u http://some.crap.wordpress/whatever.gif
```

returns a url to the anonymous photo upload

pipe it through `xsel` or `pbcopy` for auto copy to clipboard

`imgup sloth.png | pbcopy`


Setup
===

1. stack install

2. get a client_id for imgur

3. `echo "CLIENT_ID=<YOUR CLIENT-ID>" > ~/.imgup`
