# extract-audio-data-uris

Grabs audio data URIs from HTML and extracts them into separate files.


## Usage

The program expects HTML as input to STDIN and outputs HTML to STDOUT.

It takes two arguments; a URI prefix and an output directory. Assuming you have
an HTML file named `out.html`, a directory called `_site` with a subpath
`static/notebooks/datestamp`, and a website where that `_site` directory is
hosted at the root:

```sh
cat out.html | stack exec extract-audio-data-uris /static/notebooks/datestamp _site/static/notebooks/datestamp/ > datestamp.html
```
