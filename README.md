#LambdaCube 3D
  LambdaCube 3D is a domain specific language and library that makes it possible to program GPUs in a purely functional style.  
  You can find more info in the development blog. http://lambdacube3d.wordpress.com/

##Lambdacube-core

  Lambdacube-core contains the graphics EDSL and the OpenGL 3.2 backend.

    Install:
        cd lambdacube-core
        cabal install

##Q3Demo
  Quake III level viewer demo application for lambdacube-core.
  It requires the *.pk3 data files from original Quake 3 demo or full version of game.
  Addtionally custom made game levels are available from http://lvlworld.com. 
  During startup the application will find all .pk3 files available in the current directory.
  The map name can be given as an argument of the q3demo executable.
    
    Install:
        cd q3demo
        cabal install

    Example usage:
        q3demo q3dm1

##Stunts
  A revival of the classic racing game Stunts to serve as a non-toy-sized example for LambdaCube.
  It depends on the Bullet physics engine haskell binding. It is available from git repository only.    
  *git clone git://github.com/csabahruska/bullet.git*    
  The bullet installation instructions can be found in *bullet/README*.
  In order to make the stunts demo work, you need to download the original game as per the instructions given by the program.
    
    Install:
        cd hStunts
        cabal install

    Usage:
        stunts

##Screenshots    
![Stunts demo 1](https://github.com/csabahruska/lc-dsl/raw/master/lc-stunts-1.png)

![Stunts demo 2](https://github.com/csabahruska/lc-dsl/raw/master/lc-stunts-2.png)

![Quake III level viewer](https://github.com/csabahruska/lc-dsl/raw/master/lc-q3.png)

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/e9d765cd68f3f5ed77fddc1103cc37a0 "githalytics.com")](http://githalytics.com/csabahruska/lc-dsl)
