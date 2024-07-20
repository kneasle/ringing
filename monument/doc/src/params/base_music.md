# `base_music`

**Defaults to "default".**

Like [`base_calls`](base_calls.md), `base_music` adds a set of basic music definitions to the search.

`base_music` has three values: `"none"`, `"default"` and `"complib"`.

### Option 1: Only use user-defined music

```toml
base_music = "none"
```

Adds no music to the search; the only music will be what you explicitly specify.

### Option 2: Use Monument's default music selection

```toml
base_music = "default"
```

For most stages, this adds a basic music profile intended to be a sane default for modern music
tastes.  This roughly follows the 'headline' music in CompLib (i.e. the summary line shown below
every composition.

This is currently supported for
[Minor](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/default-music-minor.toml),
[Triples](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/default-music-triples.toml),
[Major](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/default-music-major.toml),
[Royal](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/default-music-royal.toml) and
[Maximus](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/default-music-maximus.toml).
You can click on each stage to see how that stage's music is defined.

### Option 3: Use CompLib's music scoring

```toml
base_music = "complib"
```

For most stages, this adds a music profile that exactly follows the complib music scoring (excluding
wraps, currently).

This is currently supported for
[Minor](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/complib-music-minor.toml),
[Triples](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/complib-music-triples.toml),
[Major](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/complib-music-major.toml),
[Royal](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/complib-music-royal.toml) and
[Maximus](https://github.com/kneasle/ringing/tree/master/monument/cli/src/music/complib-music-maximus.toml).
You can click on each stage to see how that stage's music is defined.
