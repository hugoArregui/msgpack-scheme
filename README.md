# MessagePack for Scheme

An implementation of [MessagePack](http://msgpack.org/) for Scheme.

## Specification

pack procedures

```scheme
(pack-uint port value)
(pack-sint port value)
(pack-float port value)
(pack-double port value)
(pack-raw port value)
(pack-array port value)
(pack-map port value)
(pack port value)
```

mappers
```scheme
(raw->string/mapper e)
```

unpack procedures
```scheme
(unpack port #!optional (mapper identity))
```
