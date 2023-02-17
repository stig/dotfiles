{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }: let
    arch = "aarch64-darwin";
  in {
    defaultPackage.${arch} = home-manager.defaultPackage.${arch};

    homeConfigurations.stig = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${arch};
      modules = [ ./home.nix ];
    };
  };
}
