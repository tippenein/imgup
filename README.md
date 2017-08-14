imgup
=====

Upload an image anonymously to imgur

### Screenshots

Upload your most recent screenshot anonymously with the `--screenshot` flag

Setup
===

1. stack install

2. get a client_id for imgur

3. `echo "CLIENT_ID=<YOUR CLIENT-ID>" > ~/.imgup`

### Usage

```
imgup --screenshot |pbcopy
```

![screenshot example](screenshot_example.png)


By default, this checks for the most recent match to ~/Desktop/\*.png

You can change this by adding fields to your ~/.imgup config

```
CLIENT_ID=yourclientid
PATTERN=*_scrot.png
FROM_DIRECTORY=screenies
```

This tool has other basic functionality too.

### Filesystem

You could manually upload any file on your filesystem in the same way

```
imgup ~/Desktop/sloth-happy.png
```

### Reuploaded to Imgur

You can also hand it a url (with the `-u` flag) to reupload an image to imgur.

```
imgup -u http://some.crap.wordpress/whatever.gif
```


Note
---

pipe it through `xsel` or `pbcopy` for auto copy to clipboard

`imgup sloth.png | pbcopy`

