{
  description = "Home Manager & Nix Darwin configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    clojure-lsp.url = "github:clojure-lsp/clojure-lsp";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = inputs@{ nixpkgs, home-manager, darwin, ... }: {
    darwinConfigurations = {
      cci-stig-9c7j1 = darwin.lib.darwinSystem {
        system  = "aarch64-darwin";
        modules = [ ./configuration.nix ];
      };
    };
    homeConfigurations.stig = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages."aarch64-darwin";
      modules = [
        ({ config, pkgs, ... }: { nixpkgs.overlays = [
                                    inputs.clojure-lsp.overlays.default
                                    inputs.emacs-overlay.overlays.default
                                  ];
                                })
        ./home.nix
      ];
    };
  };
}
