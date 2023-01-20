# arduboy-ada
Ada Framework for the Arduboy game console

This library is heavily based on the [Arduboy2 library by Scott
Allen](https://github.com/MLXXXp/Arduboy2).

# Build and try the example

Clone the repo and go to the example directory:
```console
$ git clone https://github.com/Fabien-Chouteau/arduboy-ada
$ cd arduboy-ada/example
```

Build with [Alire](https://alire.ada.dev):
```console
$ alr build
```

Run the `.hex` in an emulator or on the Arduboy.
For instance with RetroArch and [arduous](https://github.com/libretro/arduous):
```console
$ retroarch -L <PATH_TO_ARDUOUS_BUILD>/arduous_libretro.so bin/example.hex
```
