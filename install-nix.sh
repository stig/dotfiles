#!/bin/sh

# This script installs the Nix package manager on your system by
# downloading a binary distribution and running its installer script
# (which in turn creates and populates /nix).

{ # Prevent execution if this script was only partially downloaded
oops() {
    echo "$0:" "$@" >&2
    exit 1
}

umask 0022

tmpDir="$(mktemp -d -t nix-binary-tarball-unpack.XXXXXXXXXX || \
          oops "Can't create temporary directory for downloading the Nix binary tarball")"
cleanup() {
    rm -rf "$tmpDir"
}
trap cleanup EXIT INT QUIT TERM

require_util() {
    command -v "$1" > /dev/null 2>&1 ||
        oops "you do not have '$1' installed, which I need to $2"
}

case "$(uname -s).$(uname -m)" in
    Linux.x86_64)
        hash=766246f85c68de879856a96c6b41fde2a94452ae030583da59fca9a982691de8
        path=qlpjzrbphrm2yq215lszwwkqqv77g0wc/nix-2.6.0-x86_64-linux.tar.xz
        system=x86_64-linux
        ;;
    Linux.i?86)
        hash=a782589ee5ae028b9f281d157e5a4191a322ec2f9ef034088d04a8e6b956aef1
        path=zgs26gkky99mc7ffifdgyjclnpcjh6p7/nix-2.6.0-i686-linux.tar.xz
        system=i686-linux
        ;;
    Linux.aarch64)
        hash=b7d05160f25a05ddf4a66b5427d160de71ca741cc12ef39b50c0bbdac184a4cb
        path=k7n53gs1jy0zgnahd56fs1nxcbp2wykx/nix-2.6.0-aarch64-linux.tar.xz
        system=aarch64-linux
        ;;
    Linux.armv6l_linux)
        hash=5e0675bceb91ae1ed5590ce500b62af2341367d1c00b2a650ce56ec6fe97ce00
        path=r7p3v8j4m216aw1a2cv8iqbnpb0z48yp/nix-2.6.0-armv6l-linux.tar.xz
        system=armv6l-linux
        ;;
    Linux.armv7l_linux)
        hash=aee888c395da0e6b305df712cbbdafa525ddbd18808cecc25dbdc4431ee41509
        path=2yi90ll2z6if0nqqg71ihzzwx6nlsdq6/nix-2.6.0-armv7l-linux.tar.xz
        system=armv7l-linux
        ;;
    Darwin.x86_64)
        hash=9e38fbc023e7de18775dd71a0ac29854ca7a6a33c3a0bfa840a30bb5693c60da
        path=a7d65zp98030ndg76cp840mgx6nn4yp9/nix-2.6.0-x86_64-darwin.tar.xz
        system=x86_64-darwin
        ;;
    Darwin.arm64|Darwin.aarch64)
        hash=f21e7a3547923fc47c581dd02583af47985d8377ab80744c0ed5df0aea289ad2
        path=xf89mxrwq82gj7k8ncvb7z7faxv5ya6w/nix-2.6.0-aarch64-darwin.tar.xz
        system=aarch64-darwin
        ;;
    *) oops "sorry, there is no binary distribution of Nix for your platform";;
esac

# Use this command-line option to fetch the tarballs using nar-serve or Cachix
if [ "${1:-}" = "--tarball-url-prefix" ]; then
    if [ -z "${2:-}" ]; then
        oops "missing argument for --tarball-url-prefix"
    fi
    url=${2}/${path}
    shift 2
else
    url=https://releases.nixos.org/nix/nix-2.6.0/nix-2.6.0-$system.tar.xz
fi

tarball=$tmpDir/nix-2.6.0-$system.tar.xz

require_util tar "unpack the binary tarball"
if [ "$(uname -s)" != "Darwin" ]; then
    require_util xz "unpack the binary tarball"
fi

if command -v curl > /dev/null 2>&1; then
    fetch() { curl -L "$1" -o "$2"; }
elif command -v wget > /dev/null 2>&1; then
    fetch() { wget "$1" -O "$2"; }
else
    oops "you don't have wget or curl installed, which I need to download the binary tarball"
fi

echo "downloading Nix 2.6.0 binary tarball for $system from '$url' to '$tmpDir'..."
fetch "$url" "$tarball" || oops "failed to download '$url'"

if command -v sha256sum > /dev/null 2>&1; then
    hash2="$(sha256sum -b "$tarball" | cut -c1-64)"
elif command -v shasum > /dev/null 2>&1; then
    hash2="$(shasum -a 256 -b "$tarball" | cut -c1-64)"
elif command -v openssl > /dev/null 2>&1; then
    hash2="$(openssl dgst -r -sha256 "$tarball" | cut -c1-64)"
else
    oops "cannot verify the SHA-256 hash of '$url'; you need one of 'shasum', 'sha256sum', or 'openssl'"
fi

if [ "$hash" != "$hash2" ]; then
    oops "SHA-256 hash mismatch in '$url'; expected $hash, got $hash2"
fi

unpack=$tmpDir/unpack
mkdir -p "$unpack"
tar -xJf "$tarball" -C "$unpack" || oops "failed to unpack '$url'"

script=$(echo "$unpack"/*/install)

[ -e "$script" ] || oops "installation script is missing from the binary tarball!"
export INVOKED_FROM_INSTALL_IN=1
"$script" "$@"

} # End of wrapping
