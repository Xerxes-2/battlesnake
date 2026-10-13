{
  lib,
  buildDotnetModule,
  dotnetCorePackages,
}:

buildDotnetModule {
  pname = "snake-server";
  version = "0.1.0";

  # 只把构建需要的文件放进 src：改测试、bench 不触发重编
  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../Directory.Build.props
      ../src
    ];
  };

  projectFile = "src/Server/Server.fsproj";
  # 更新：nix build .#default.fetch-deps && ./result nix/deps.json
  nugetDeps = ./deps.json;

  dotnet-sdk = dotnetCorePackages.sdk_10_0;
  dotnet-runtime = dotnetCorePackages.aspnetcore_10_0;

  # 项目默认同时编 net10/net11（本地性能对比用），这里只有 SDK 10
  dotnetFlags = [ "-p:SnakeTargetFrameworks=net10.0" ];
  # 用的仍是复数的 TargetFrameworks，publish 认为是多目标，要显式指定框架
  dotnetBuildFlags = [
    "--framework"
    "net10.0"
  ];
  dotnetInstallFlags = [
    "--framework"
    "net10.0"
  ];

  executables = [ "snake-server" ];

  meta = {
    description = "F# Battlesnake 服务器";
    mainProgram = "snake-server";
    # fetch-deps 按这里的平台各拉一份 RID 相关包（ReadyToRun 的 crossgen2）
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
    ];
  };
}
