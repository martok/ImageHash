[*main*]
tasks=read_version,compile,package

[read_version]
tool=lazversion
project=ImageHash.lpi
setenv=VERSION

[compile]
tool=lazbuild
project=ImageHash.lpi
build=clean
mode=Default

[package]
tool=zip
filename=imagehash-${VERSION}-win32.zip
files=package.files

[package.files]
README.md=
bin_i386/ImageHash.exe=
bin_i386/libturbojpeg-0.dll=
