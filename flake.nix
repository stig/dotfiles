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
        modules = [
          ({ config, pkgs, ... }: { nixpkgs.overlays = [
                                      inputs.clojure-lsp.overlays.default
                                      inputs.emacs-overlay.overlays.default
                                    ];
                                  })
          ./configuration.nix
          home-manager.darwinModules.home-manager {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.stig = import ./home.nix;
          }
        ];
      };
    };
  };
}
