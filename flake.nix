{
  description = "F# Battlesnake 服务器";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      packages = forAll (pkgs: rec {
        snake-server = pkgs.callPackage ./nix/package.nix {
          rev = self.shortRev or self.dirtyShortRev or "unknown";
        };
        default = snake-server;
      });

      # 只出包和服务模块；监听哪个端口、怎么暴露到公网是部署机自己的事
      nixosModules.default = import ./nix/module.nix self;

      devShells = forAll (pkgs: {
        default = pkgs.mkShell {
          packages = [
            (pkgs.dotnetCorePackages.combinePackages [
              pkgs.dotnetCorePackages.sdk_10_0
              pkgs.dotnetCorePackages.sdk_11_0
            ])
          ];
        };
      });

      formatter = forAll (pkgs: pkgs.nixfmt);
    };
}
