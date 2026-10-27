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
            pkgs.fantomas
          ];
        };
      });

      # nix flake check：F# 格式与 .editorconfig 一致（fantomas 版本和 .config/dotnet-tools.json 对齐）
      checks = forAll (pkgs: {
        format = pkgs.runCommand "snake-format" { nativeBuildInputs = [ pkgs.fantomas ]; } ''
          cd ${self}
          HOME=$TMPDIR fantomas --check src arena bench tests
          touch $out
        '';
      });

      formatter = forAll (pkgs: pkgs.nixfmt);
    };
}
