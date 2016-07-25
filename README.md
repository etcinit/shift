# shift

A changelog generator

## Usage

This project is currently a prototype, and mainly works if you project meets
the following criteria:

- It is on Git.
- Commits follow the Angular commit convention.
- Numeric tags (vX.X.X.X, vX.X.X, and others).
- For URLs to authors and commits, it will need to be hosted on GitHub.

If it does, you can build and install this project using Stack:

```
stack build --copy-bins
```

Once `shift`, is in your `PATH`, you can invoke it according to where your
repository is hosted:

- **GitHub:**

```
shift generate -t github --github-token XXX --github-owner etcinit --github-repository shift
```

- **Local/Other:** `shift generate -t git`.

To save to a `CHANGELOG.md` file: `shift generate -t git > CHANGELOG.md`.

## TODO

A lot.

- Other hosting types: Phabricator, BitBucket.
- Support more commit conventions.
- Templates.
- Releases.
