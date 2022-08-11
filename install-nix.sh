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
        hash=21f14aecde1f00e7c463f75ddb819a03109e1ef108b82c4305c0bb6fa58a01a0
        path=ack4h7n5kyyk6hz5778qar4irh0i9lda/nix-2.10.3-x86_64-linux.tar.xz
        system=x86_64-linux
        ;;
    Linux.i?86)
        hash=29fdf58e3194c7fb9c4aa171aedc1fbe42da34c1e996845f025dce16ea3e8a1c
        path=yhipirvqxnldx5gk8z9gz3r3bbm556dy/nix-2.10.3-i686-linux.tar.xz
        system=i686-linux
        ;;
    Linux.aarch64)
        hash=0f135aa6ab94b24d1e0dcac37ec16b0beffde4d0bb767eb8ec1b1055c1c9316a
        path=apnk89xmzr3bf3zbqh5xa3b4hg6197w3/nix-2.10.3-aarch64-linux.tar.xz
        system=aarch64-linux
        ;;
    Linux.armv6l_linux)
        hash=fcc45b9c8b67c6fe744dfa88fa697892be5bc9bd4082617c26b8a57ada1ee4f5
        path=03k0b9hyiq4pzi9vpn3y9pfp388gg5l5/nix-2.10.3-armv6l-linux.tar.xz
        system=armv6l-linux
        ;;
    Linux.armv7l_linux)
        hash=44bef998a033a8411d3daa8895064b633f8014da720aa2f0c7187cab386a6a75
        path=3cxqaqihixswaazsz0nraw082n6hqp9h/nix-2.10.3-armv7l-linux.tar.xz
        system=armv7l-linux
        ;;
    Darwin.x86_64)
        hash=41c9044520a721b4c89e1b7ad2846d4b5cfc47c566cf7caf6abefeed971a24cf
        path=f0na06dyd5b4vmk6gxfjgsg0r1brd9r5/nix-2.10.3-x86_64-darwin.tar.xz
        system=x86_64-darwin
        ;;
    Darwin.arm64|Darwin.aarch64)
        hash=dc24287a383dbb28f06c5cdec537709843f259819dd1518c0e0df44870df5b38
        path=czpf9w60r6v4bkxh54dzahm4psnrycc7/nix-2.10.3-aarch64-darwin.tar.xz
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
    url=https://releases.nixos.org/nix/nix-2.10.3/nix-2.10.3-$system.tar.xz
fi

tarball=$tmpDir/nix-2.10.3-$system.tar.xz

require_util tar "unpack the binary tarball"
if [ "$(uname -s)" != "Darwin" ]; then
    require_util xz "unpack the binary tarball"
fi

if command -v curl > /dev/null 2>&1; then
    fetch() { curl --fail -L "$1" -o "$2"; }
elif command -v wget > /dev/null 2>&1; then
    fetch() { wget "$1" -O "$2"; }
else
    oops "you don't have wget or curl installed, which I need to download the binary tarball"
fi

echo "downloading Nix 2.10.3 binary tarball for $system from '$url' to '$tmpDir'..."
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
