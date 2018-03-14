# corenlp-parser

Launches CoreNLP and parses the JSON output. See `NLP.CoreNLP`
haddocks for the documentation (or read the source) http://hackage.haskell.org/package/corenlp-parser

Building via:

```
stack build
```

## TODO

- [ ] create types for sum POS tags and alike instead of just `Text`
- [ ] add a way to launch CoreNLP as a service somehow
- [ ] add CI/Travis config
- [ ] add proper tests, separate them in a tests executable
