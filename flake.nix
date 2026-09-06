{
  description = "YouTube front-end for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkYeetube = system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacs = pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          # Makefile owns the source/test manifest, including reload order.
          manifest = name:
            lib.splitString " " (builtins.head
              (builtins.match ".*\n${name} = ([^\n]+)\n.*"
                (builtins.readFile ./Makefile)));
          source = lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions ([ ./Makefile ] ++
              map (name: ./. + "/${name}")
                (manifest "SRCS" ++ manifest "TESTS" ++ manifest "FIXTURES"));
          };

          keymapPopupVersion = "0.4.1";

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = pkgs.fetchurl {
              url = "https://elpa.gnu.org/packages/keymap-popup-${keymapPopupVersion}.tar.lz";
              hash = "sha256-3Xs51u3K6LT3xRqRn82FrGfoKg9oXBRZSojpFAMmjXA=";
            };
            nativeBuildInputs = [ pkgs.lzip ];
            packageRequires = [ ];
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            keymapPopup
            epkgs.compat
          ]);

          tests = pkgs.stdenv.mkDerivation {
            pname = "yeetube-tests";
            version = "git";
            src = source;
            nativeBuildInputs = [
              emacsWithPackages
              pkgs.gnumake
            ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild
              unset EMACSDATA EMACSDOC EMACSLOADPATH EMACSPATH GREP_OPTIONS
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/share"
              export XDG_STATE_HOME="$TMPDIR/state"
              mkdir -p "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" \
                "$XDG_DATA_HOME" "$XDG_STATE_HOME"
              EMACS_CMD=emacs \
                YEETUBE_ENV_WRAPPED=1 \
                make dev
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              touch $out/tests-passed
              runHook postInstall
            '';
          };
        in {
          inherit emacs emacsWithPackages keymapPopup pkgs tests;
        };
    in {
      checks = forAllSystems (system:
        let yeetube = mkYeetube system;
        in {
          test = yeetube.tests;
        });

      devShells = forAllSystems (system:
        let yeetube = mkYeetube system;
        in {
          default = yeetube.pkgs.mkShell {
            packages = with yeetube.pkgs; [
              cacert
              yeetube.emacsWithPackages
              git
              gnumake
            ];

            shellHook = ''
              export EMACS_CMD=emacs
            '';
          };
        });
    };
}
