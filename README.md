# LambdaCube 3D as Embedded Domain Specific Language in Haskell.

Check the latest system: http://lambdacube3d.com

# LambdaCube 3D
  LambdaCube 3D is a domain specific language and library that makes it possible to program GPUs in a purely functional style.  
  You can find more info in the development blog. http://lambdacube3d.wordpress.com/

## Setup

#### On **Linux** install the following libraries.
   i.e. on Ubuntu:
   ```
   sudo apt install libgl1-mesa-dev libxi-dev libxcursor-dev libxinerama-dev libxrandr-dev zlib1g-dev libpulse-dev
   ```
   For other Linux distributions make sure the corresponing packages are installed.

   *These libraries required for OpenGL development.*


#### Compile & Run:

To compile you will need [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

```
stack setup
stack build

stack exec -- lambdacube-hello
stack exec -- lambdacube-shadowmapping
stack exec -- lambdacube-cubemap
stack exec -- lambdacube-convolutionfilter
```

## Lambdacube-edsl

  Lambdacube-core contains the graphics EDSL and the OpenGL 3.2 backend.

## Q3Demo
  Quake III level viewer demo application for lambdacube-core.
  It requires the *.pk3 data files from original Quake 3 demo or full version of game.
  Addtionally custom made game levels are available from http://lvlworld.com. 
  During startup the application will find all .pk3 files available in the current directory.
  The map name can be given as an argument of the q3demo executable.

## Stunts
  A revival of the classic racing game Stunts to serve as a non-toy-sized example for LambdaCube.
  It depends on the Bullet physics engine haskell binding. It is available from git repository only.    
  *git clone git://github.com/csabahruska/bullet.git*    
  The bullet installation instructions can be found in *bullet/README*.
  In order to make the stunts demo work, you need to download the original game as per the instructions given by the program.


## Screenshots

![Stunts demo 1](https://github.com/csabahruska/lc-dsl/raw/master/lc-stunts-1.png)

![Stunts demo 2](https://github.com/csabahruska/lc-dsl/raw/master/lc-stunts-2.png)

![Quake III level viewer](https://github.com/csabahruska/lc-dsl/raw/master/lc-q3.png)
