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
        hash=818859b1e52af626b39d9e73e0b0f25e8b55151dfaeb50abd4b8e5cd1bd016a9
        path=xn4wq4nnqx1fbisijnvgv3akp33b6wan/nix-2.7.0-x86_64-linux.tar.xz
        system=x86_64-linux
        ;;
    Linux.i?86)
        hash=af27ea773e9d702586a83fd92d6d3ed795d199f043573c54a166500de7006e35
        path=r1lww4c9r6qpxwvv3z2ffl991d03bd13/nix-2.7.0-i686-linux.tar.xz
        system=i686-linux
        ;;
    Linux.aarch64)
        hash=cea9041ad97d3f77c887c402bd3e012ce2ec0971c39f65c21df8e0b108aa2b60
        path=ki93l6h1nr02na7n8nmx1m9q2r7k4rh8/nix-2.7.0-aarch64-linux.tar.xz
        system=aarch64-linux
        ;;
    Linux.armv6l_linux)
        hash=8a83bb6716ba459d65319ebf671900c350675fecd9c972a65d1d437530c93e97
        path=a6nrdn7d31bs831zxb4m0mkpy9g3lzmn/nix-2.7.0-armv6l-linux.tar.xz
        system=armv6l-linux
        ;;
    Linux.armv7l_linux)
        hash=6ef130a6a3bf80a7de854eae94734a30b3aff9190909957848e559906d09bb67
        path=88xhvwmlfvir4jwmw6zbglj9smh3xrzi/nix-2.7.0-armv7l-linux.tar.xz
        system=armv7l-linux
        ;;
    Darwin.x86_64)
        hash=4a6f06aabae38c4659c7b7459e737a26b7f6fa91b04352a0df2ed830b3767ad0
        path=iz6ngv4sbac92hvikdnxabilpk36z406/nix-2.7.0-x86_64-darwin.tar.xz
        system=x86_64-darwin
        ;;
    Darwin.arm64|Darwin.aarch64)
        hash=9d2b43abe8797748893b119864f7d1ffc11afef120e33c9264f4973c95c65324
        path=0nzkcjd4l7n42cjl0if9kh09wwxcsjbh/nix-2.7.0-aarch64-darwin.tar.xz
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
    url=https://releases.nixos.org/nix/nix-2.7.0/nix-2.7.0-$system.tar.xz
fi

tarball=$tmpDir/nix-2.7.0-$system.tar.xz

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

echo "downloading Nix 2.7.0 binary tarball for $system from '$url' to '$tmpDir'..."
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
