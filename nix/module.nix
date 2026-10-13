self:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.battlesnake;
in
{
  options.services.battlesnake = {
    enable = lib.mkEnableOption "F# Battlesnake 服务器";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.default;
      description = "要运行的 snake-server 包。";
    };

    host = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = "监听地址。默认只监听本机，交给反代或隧道暴露。";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8000;
      description = "监听端口。";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "在 NixOS 防火墙放行 port（直连暴露时用，host 也要改成 0.0.0.0）。";
    };
  };

  config = lib.mkIf cfg.enable {
    networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];

    systemd.services.battlesnake = {
      description = "Battlesnake server";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      environment = {
        HOST = cfg.host;
        PORT = toString cfg.port;
        DOTNET_CLI_TELEMETRY_OPTOUT = "1";
      };

      serviceConfig = {
        ExecStart = lib.getExe cfg.package;
        Restart = "always";
        RestartSec = 2;

        # 无状态服务，不需要任何持久目录
        DynamicUser = true;
        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        LockPersonality = true;
        # 不能开 MemoryDenyWriteExecute：.NET JIT 需要可写可执行内存
      };
    };
  };
}
