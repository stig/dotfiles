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
        hash=beaec0f28899c22f33adbe30e4ecfceef87b797278c5210ee693e22e9719dfb4
        path=fxvid1mwg2z80lcw0523wiiwv671r0ji/nix-2.13.2-x86_64-linux.tar.xz
        system=x86_64-linux
        ;;
    Linux.i?86)
        hash=6039e18a94ca822a5403471ba2fa00ea7d9e9c04734222a63408faeb4329e127
        path=zz8z04v5q6m1mf4qw2v2rgqydi7836ad/nix-2.13.2-i686-linux.tar.xz
        system=i686-linux
        ;;
    Linux.aarch64)
        hash=4ae275a46a2441d3459ae389a90ce6e8f7eff12c2a084b2d003ba6f8d0899603
        path=15ibgj3h767d392m685iyxl1jm392gmj/nix-2.13.2-aarch64-linux.tar.xz
        system=aarch64-linux
        ;;
    Linux.armv6l)
        hash=1125d25163a2f9801a07ac30a94d0afce70077822861dfbb8af0c837d18af311
        path=ravm1dlbmw9v0ln3q0d4h8bp7x6w3ybl/nix-2.13.2-armv6l-linux.tar.xz
        system=armv6l-linux
        ;;
    Linux.armv7l)
        hash=41c6e05a09358465a30cbbc54e3c1daae7a7a0e27f011747a2b113cf2a5a9dab
        path=580bgh3frkh2pw43wqgh175jw9rq71mk/nix-2.13.2-armv7l-linux.tar.xz
        system=armv7l-linux
        ;;
    Darwin.x86_64)
        hash=f55b40bc0630c7503cc0258076d7ba2d854771e4e12f098815316485768feef2
        path=f4kwqapn0nmxffffxbhi2pzaddn8gdps/nix-2.13.2-x86_64-darwin.tar.xz
        system=x86_64-darwin
        ;;
    Darwin.arm64|Darwin.aarch64)
        hash=3ec2420a34732130a20fb3f360c294ac8fb17fbeba505ecfbbd75ef0e60d04e0
        path=nksfwg252vagxdsyq6b6r9bdq824d02m/nix-2.13.2-aarch64-darwin.tar.xz
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
    url=https://releases.nixos.org/nix/nix-2.13.2/nix-2.13.2-$system.tar.xz
fi

tarball=$tmpDir/nix-2.13.2-$system.tar.xz

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

echo "downloading Nix 2.13.2 binary tarball for $system from '$url' to '$tmpDir'..."
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
