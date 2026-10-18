# Changelog

## [2.2.0] - 2026-10-18

### Changed

- Support both modes of `toggle-truncate-lines`. This means that `M-x kill-line` won't cross wrapped lines anymore, but `C-k` might, depending on the shell. ([`04c910a`](https://github.com/szermatt/mistty/commit/04c910a))

### Added

- Support mouse click and drag reporting when using `alacritty`, including `OSC 133;A;click_events=1`, with a clickable mode-line indicator. Documented in [**Mouse Support**](https://mistty.readthedocs.io/en/latest/usage.html#mouse-support). ([`0603d96`](https://github.com/szermatt/mistty/commit/0603d96), [`48ecdfb`](https://github.com/szermatt/mistty/commit/48ecdfb), [`baa298b`](https://github.com/szermatt/mistty/commit/baa298b))
- Add customizable mode-line tag themes (`mistty-tag-theme`) with optional [**nerd-icons**](https://github.com/rainstormstudio/nerd-icons.el) integration. Mentioned in the [FAQ](https://mistty.readthedocs.io/en/latest/faq.html). ([`2bb4c22`](https://github.com/szermatt/mistty/commit/2bb4c22), [`9392d51`](https://github.com/szermatt/mistty/commit/9392d51), [`411fbaf`](https://github.com/szermatt/mistty/commit/411fbaf))
- Support the **Kitty Keyboard Protocol** when using `alacritty` ([`b63a7ac`](https://github.com/szermatt/mistty/commit/b63a7ac), [`b68ef2e`](https://github.com/szermatt/mistty/commit/b68ef2e), [`d44315c`](https://github.com/szermatt/mistty/commit/d44315c), [`55ac9d3`](https://github.com/szermatt/mistty/commit/55ac9d3))
- Control terminal size using (`mistty-fit-to-window`, `mistty-grow-vertically`, `mistty-shrink-vertically`, and `mistty-toggle-track-window`). Documented in [**Terminal Size**](https://mistty.readthedocs.io/en/latest/usage.html#terminal-size). ([`3d479d9`](https://github.com/szermatt/mistty/commit/3d479d9), [`2a385ff`](https://github.com/szermatt/mistty/commit/2a385ff))
- `mistty-about` displays the version and expected module name ([`86b265a`](https://github.com/szermatt/mistty/commit/86b265a))

### Fixed

- Use correct key sequences for `alacritty`, matching its terminfo ([`85aa3a8`](https://github.com/szermatt/mistty/commit/85aa3a8), [`20821c1`](https://github.com/szermatt/mistty/commit/20821c1))
- Adapt terminal size for tall characters. Documented in [Terminal Size](https://mistty.readthedocs.io/en/latest/usage.html#terminal-size). ([`66af412`](https://github.com/szermatt/mistty/commit/66af412), [`6dc2134`](https://github.com/szermatt/mistty/commit/6dc2134))
- Limit width of terminal to avoid issues with wide character at end of line  ([`699a2ce`](https://github.com/szermatt/mistty/commit/699a2ce))
- Preserve column alignment after wide Unicode characters ([`79488bd`](https://github.com/szermatt/mistty/commit/79488bd))
- Fix color at the end of lines when using `alacritty` ([`d3e6c2d`](https://github.com/szermatt/mistty/commit/d3e6c2d))
- Declare module functions in `mistty-term-alacritty.el` to fix byte-compilation without the module installed ([`a016260`](https://github.com/szermatt/mistty/commit/a016260))

## [2.1.0] - 2026-10-03

_If you are upgrading: please see the [documentation on modes and keyboard mapping](https://mistty.readthedocs.io/en/latest/usage.html#terminal-zone-modes-and-keyboard)._

### Changed

- **Breaking:** keep fullscreen terminal in the same buffer when using `alacritty` ([`af8fd96`](https://github.com/szermatt/mistty/commit/af8fd96), [`a4ab75b`](https://github.com/szermatt/mistty/commit/a4ab75b), [`d84603f`](https://github.com/szermatt/mistty/commit/d84603f))
- Update mode-line to make the different terminal modes clearer ([`bb5711c`](https://github.com/szermatt/mistty/commit/bb5711c))
- Rename `mistty-send-key-sequence` to `mistty-capture-keyboard` with an obsolete alias ([`09a3498`](https://github.com/szermatt/mistty/commit/09a3498))
- Rename `mistty-forbid-edit-map` to `mistty-special-prompt-map` with an obsolete alias ([`ed1444e`](https://github.com/szermatt/mistty/commit/ed1444e))

### Added

- Add `mistty-toggle-keymap` to switch between prompt and fullscreen maps ([`bb5711c`](https://github.com/szermatt/mistty/commit/bb5711c), [`3c04288`](https://github.com/szermatt/mistty/commit/3c04288))
- Add `mistty-exit-capture-keyboard-key` option to configure the exit key for keyboard capture ([`62994ad`](https://github.com/szermatt/mistty/commit/62994ad))

### Fixed

- Always use bracketed paste for `yank`, `yank-pop`, and others ([#73](https://github.com/szermatt/mistty/issues/73)) ([`3212ef6`](https://github.com/szermatt/mistty/commit/3212ef6))
- Clean up `PROMPT-SP` hack immediately ([`03381c6`](https://github.com/szermatt/mistty/commit/03381c6))
- Deal with `eterm` sometimes leaving process mark before the home marker ([`b07465e`](https://github.com/szermatt/mistty/commit/b07465e))

## [2.0.2] - 2026-09-24

### Added

- Document how to configure [colors](https://mistty.readthedocs.io/en/latest/usage.html#colors) ([`d14cb03`](https://github.com/szermatt/mistty/commit/d14cb03))
- Propose reasonable values when customizing `mistty-alacritty-term-name` ([`d02e239`](https://github.com/szermatt/mistty/commit/d02e239))

### Fixed

- Fix support for bright variants of ANSI colors when using `alacritty` ([#72](https://github.com/szermatt/mistty/issues/72)) ([`f5792ee`](https://github.com/szermatt/mistty/commit/f5792ee))

## [2.0.1] - 2026-09-16

_This release introduces a new recommended terminal backend based on [Alacritty](https://alacritty.org/index.html) (selected via `mistty-terminal-type` and installed with `mistty-install` or `mistty-install-dwim`; see the [module documentation](https://mistty.readthedocs.io/en/latest/usage.html#module-with-alacritty)). It replaces 2.0.0, which had a broken `mistty-install`._

### Added

- Add `alacritty` virtual terminal backend as an alternative to Emacs built-in `eterm` ([#67](https://github.com/szermatt/mistty/issues/67), [#69](https://github.com/szermatt/mistty/pull/69)) ([`e8063ef`](https://github.com/szermatt/mistty/commit/e8063ef))
- Add `mistty-term-mode-hook` to allow customizing hooks for `eterm` separately from `mistty` ([#63](https://github.com/szermatt/mistty/issues/63)) ([`1418f8f`](https://github.com/szermatt/mistty/commit/1418f8f))
- Support hiding and showing cursor in fullscreen mode ([`24df3c5`](https://github.com/szermatt/mistty/commit/24df3c5))
- Support Fish 4.1 and Emacs 31 ([#60](https://github.com/szermatt/mistty/issues/60), [#65](https://github.com/szermatt/mistty/issues/65)) ([`5ec7a2e`](https://github.com/szermatt/mistty/commit/5ec7a2e), [`298549f`](https://github.com/szermatt/mistty/commit/298549f), [`dcba7b0`](https://github.com/szermatt/mistty/commit/dcba7b0))

### Fixed

- Limit unprocessed data that can be accumulated in bytes to prevent excessive memory usage ([`27ca8f1`](https://github.com/szermatt/mistty/commit/27ca8f1))
- Hold back chunks containing incomplete escape sequences until complete ([#49](https://github.com/szermatt/mistty/issues/49)) ([`712b00f`](https://github.com/szermatt/mistty/commit/712b00f))
- Select window before recentering ([#51](https://github.com/szermatt/mistty/issues/51)) ([`d01f0a5`](https://github.com/szermatt/mistty/commit/d01f0a5))
- Use `user-error` instead of `error` when appropriate ([#48](https://github.com/szermatt/mistty/issues/48)) ([`43d4c2b`](https://github.com/szermatt/mistty/commit/43d4c2b))
- Support `CSI ? 47|1047|1049 h|l` sequences with the `eterm` terminal ([#53](https://github.com/szermatt/mistty/issues/53)) ([`82cf1ae`](https://github.com/szermatt/mistty/commit/82cf1ae))
- Avoid being confused by invalid wrap lines ([#54](https://github.com/szermatt/mistty/issues/54)) ([`ae93596`](https://github.com/szermatt/mistty/commit/ae93596))
- Improve `prompt-sp` hack detection when changing shells ([`0f468c6`](https://github.com/szermatt/mistty/commit/0f468c6))
- Fix compatibility issues with Emacs 31 ([#60](https://github.com/szermatt/mistty/issues/60)) ([`fcd757d`](https://github.com/szermatt/mistty/commit/fcd757d), [`5ec7a2e`](https://github.com/szermatt/mistty/commit/5ec7a2e), [`dcba7b0`](https://github.com/szermatt/mistty/commit/dcba7b0))
- Fix issues with Fish 4.1 and later sending sequences unsupported by `eterm` ([#65](https://github.com/szermatt/mistty/issues/65)) ([`298549f`](https://github.com/szermatt/mistty/commit/298549f), [`879abed`](https://github.com/szermatt/mistty/commit/879abed))
- Support redirects when downloading module from GitHub in `mistty-install` ([`9dfd873`](https://github.com/szermatt/mistty/commit/9dfd873))

## [1.5] - 2025-04-10

### Changed

- Move to the beginning of user input in `mistty-next-input` and `mistty-prev-input` ([`f4f718a`](https://github.com/szermatt/mistty/commit/f4f718a))

### Added

- Support [**OSC 133**](https://mistty.readthedocs.io/en/latest/usage.html#supported-osc-control-sequences) escape sequences ([#46](https://github.com/szermatt/mistty/issues/46)) ([`38b8f79`](https://github.com/szermatt/mistty/commit/38b8f79))
- Mark prompt as field when OSC 133 B codes are issued so commands like `kill-whole-line` or `meow-line` work on prompts ([`9a08f5f`](https://github.com/szermatt/mistty/commit/9a08f5f))
- Define prompts as defuns so commands like `mark-defun` or `next-defun` can navigate through older command outputs ([`205029a`](https://github.com/szermatt/mistty/commit/205029a))
- Build `imenu` index to navigate through older prompts and their output ([`1858cf3`](https://github.com/szermatt/mistty/commit/1858cf3))
- Add `mistty-allow-clearing-scrollback` option to let `reset` clear the whole buffer ([#42](https://github.com/szermatt/mistty/issues/42)) ([`daf63f8`](https://github.com/szermatt/mistty/commit/daf63f8))

### Fixed

- Fix sync marker moved by `ESC [ 2 K` used on the same line ([#47](https://github.com/szermatt/mistty/issues/47)) ([`5a04245`](https://github.com/szermatt/mistty/commit/5a04245))
- Resize terminal when nonselected window is resized ([#45](https://github.com/szermatt/mistty/issues/45)) ([`26687f3`](https://github.com/szermatt/mistty/commit/26687f3))
- Send `stty erase ^H` when starting terminal so `read` respects backspace on macOS ([#12](https://github.com/szermatt/mistty/issues/12)) ([`ad4bc9b`](https://github.com/szermatt/mistty/commit/ad4bc9b))

## [1.4] - 2025-03-07

### Changed

- **Breaking:** Kill buffer after process end by default, configurable via `mistty-at-end` ([`429cf35`](https://github.com/szermatt/mistty/commit/429cf35))
- Reuse dead MisTTY buffers with the same name ([#30](https://github.com/szermatt/mistty/issues/30)) ([`b6717bb`](https://github.com/szermatt/mistty/commit/b6717bb))

### Added

- Discover the start of [multi-line prompts in Zsh](https://mistty.readthedocs.io/en/latest/shells.html#fancy-prompts-in-zsh) ([`8292eed`](https://github.com/szermatt/mistty/commit/8292eed))

### Fixed

- Ignore unsupported control sequences sent by Fish 4.0.0 ([`162d966`](https://github.com/szermatt/mistty/commit/162d966))
- Send `^H` (BS) instead of `^?` (DEL) for backward-delete to avoid Fish 4.0.0 misinterpretation ([`83caf7c`](https://github.com/szermatt/mistty/commit/83caf7c))
- Fix detection of `i-search` on Zsh ([#31](https://github.com/szermatt/mistty/issues/31)) ([`f4f04a4`](https://github.com/szermatt/mistty/commit/f4f04a4))
- Fix prompt detection issues on Zsh ([#33](https://github.com/szermatt/mistty/issues/33)) ([`8356695`](https://github.com/szermatt/mistty/commit/8356695), [`5db4393`](https://github.com/szermatt/mistty/commit/5db4393), [`03945a4`](https://github.com/szermatt/mistty/commit/03945a4), [`8a42393`](https://github.com/szermatt/mistty/commit/8a42393))
- Deal with Zsh leaving undeletable trailing spaces at the end of a line or prompt ([#34](https://github.com/szermatt/mistty/issues/34)) ([`a4b6164`](https://github.com/szermatt/mistty/commit/a4b6164), [`6023672`](https://github.com/szermatt/mistty/commit/6023672), [`f257ea1`](https://github.com/szermatt/mistty/commit/f257ea1), [`768bdc7`](https://github.com/szermatt/mistty/commit/768bdc7))
- Fix first prompt refresh on Zsh ([`50f8d02`](https://github.com/szermatt/mistty/commit/50f8d02))

## [1.3] - 2025-01-01

### Added

- Support editing and motion in multi-line IPython prompts ([`4d046c1`](https://github.com/szermatt/mistty/commit/4d046c1), [`46b038c`](https://github.com/szermatt/mistty/commit/46b038c))
- Detect IPython `i-search` prompts ([`a28ccc1`](https://github.com/szermatt/mistty/commit/a28ccc1))
- Add `mistty-ssh` command to open an SSH instance ([`3db1d9b`](https://github.com/szermatt/mistty/commit/3db1d9b))
- Add `mistty-docker` command to open a Docker instance ([`de9da93`](https://github.com/szermatt/mistty/commit/de9da93))

### Fixed

- Fix reset behavior to avoid duplicating output or losing cursor position ([`0883b38`](https://github.com/szermatt/mistty/commit/0883b38), [`18046d5`](https://github.com/szermatt/mistty/commit/18046d5))
- Fix erratic movements in IPython ([#28](https://github.com/szermatt/mistty/issues/28)) ([`57b300c`](https://github.com/szermatt/mistty/commit/57b300c), [`10d7203`](https://github.com/szermatt/mistty/commit/10d7203))
- Fix cursor position after `backward-i-search` in Bash ([#27](https://github.com/szermatt/mistty/issues/27)) ([`d78641f`](https://github.com/szermatt/mistty/commit/d78641f))
- Avoid fake newlines throwing off buffer alignment after reset ([`34ce96f`](https://github.com/szermatt/mistty/commit/34ce96f))

## [1.2] - 2024-11-23

### Changed

- Propose a default buffer name for `C-c C-r` (`mistty-create-buffer-with-output`) ([`6c5f9ba`](https://github.com/szermatt/mistty/commit/6c5f9ba))
- Allow default colors to change as the theme changes ([`f49dce6`](https://github.com/szermatt/mistty/commit/f49dce6))
- Scroll up to show completion below point if necessary ([`6bb6025`](https://github.com/szermatt/mistty/commit/6bb6025))

### Added

- Detect Fish "search within completion" mode so arrow keys work within that mode ([`046f792`](https://github.com/szermatt/mistty/commit/046f792))
- Add [`mistty-set-EMACS`](https://mistty.readthedocs.io/en/1.2/shells.html#directory-tracking) option to control whether the `EMACS` environment variable is set for directory tracking on Bash < 4.4 ([`c868d01`](https://github.com/szermatt/mistty/commit/c868d01))

### Fixed

- Fix whole-line deletion with right prompt ([`c68709e`](https://github.com/szermatt/mistty/commit/c68709e))
- Restore original cursor after hiding it ([#25](https://github.com/szermatt/mistty/issues/25)) ([`0213f6c`](https://github.com/szermatt/mistty/commit/0213f6c))
- Set buffer mode before calling `pop-to-buffer` so mode-based rules work ([`bba7d35`](https://github.com/szermatt/mistty/commit/bba7d35))

## [1.1] - 2024-10-12

### Changed

- **Breaking:** allow TRAMP paths by default and automatically start a [TRAMP remote shell](https://mistty.readthedocs.io/en/1.1/usage.html#remote-shells-with-tramp) when called from a buffer visiting a remote directory ([`8cf8fad`](https://github.com/szermatt/mistty/commit/8cf8fad), [`73e8166`](https://github.com/szermatt/mistty/commit/73e8166))
- Prompt for a directory when calling `C-u M-x mistty-create` ([`fe57bb5`](https://github.com/szermatt/mistty/commit/fe57bb5))
- Allow simple editing commands like `yank` or `delete-word` in [backward search mode](https://mistty.readthedocs.io/en/1.1/usage.html#backward-search) ([`d51d6d9`](https://github.com/szermatt/mistty/commit/d51d6d9))
- Send arrow keys to the terminal during backward search mode ([`ef3ee11`](https://github.com/szermatt/mistty/commit/ef3ee11))

### Added

- Add `mistty-kill-buffer` and `mistty-kill-buffer-and-window` to close buffer and window after the shell exits ([#20](https://github.com/szermatt/mistty/issues/20)) ([`9c0fe22`](https://github.com/szermatt/mistty/commit/9c0fe22))
- Add `mistty-in-project-other-window` to open a project shell in another window ([#19](https://github.com/szermatt/mistty/pull/19)) ([`fdcbcd4`](https://github.com/szermatt/mistty/commit/fdcbcd4)) ([**@ROCKTAKEY**](https://github.com/ROCKTAKEY))

### Fixed

- Handle the `clear` command properly ([#20](https://github.com/szermatt/mistty/issues/20)) ([`321cba9`](https://github.com/szermatt/mistty/commit/321cba9), [`5bb65ab`](https://github.com/szermatt/mistty/commit/5bb65ab))
- Remove fake newlines outside of the terminal zone to fix `copy-region` when the window is resized ([#24](https://github.com/szermatt/mistty/issues/24)) ([`dc39f6b`](https://github.com/szermatt/mistty/commit/dc39f6b))
- Change `window-point-insertion-type` only locally ([#22](https://github.com/szermatt/mistty/issues/22)) ([`df18ebd`](https://github.com/szermatt/mistty/commit/df18ebd))
- Fix TRAMP paths generated from non-local `file:` URLs ([`e3ea40f`](https://github.com/szermatt/mistty/commit/e3ea40f), [`4e1f6f4`](https://github.com/szermatt/mistty/commit/4e1f6f4), [`d241868`](https://github.com/szermatt/mistty/commit/d241868))

## [1.0] - 2023-11-20

### Changed

- Respect `display-comint-buffer-action` in `mistty` and `mistty-create` ([`73825e4`](https://github.com/szermatt/mistty/commit/73825e4))
- Speed up commands with long output ([#7](https://github.com/szermatt/mistty/issues/7)) ([`12261f4`](https://github.com/szermatt/mistty/commit/12261f4), [`a7c7471`](https://github.com/szermatt/mistty/commit/a7c7471))
- Improve support for right prompts ([`640ba52`](https://github.com/szermatt/mistty/commit/640ba52), [`61cb757`](https://github.com/szermatt/mistty/commit/61cb757))
- Let Emacs scroll MisTTY buffers normally ([#6](https://github.com/szermatt/mistty/issues/6)) ([`d538cf4`](https://github.com/szermatt/mistty/commit/d538cf4))
- Insert replay text after the prompt if inserted before the prompt, and drop text inserted after the prompt ([`977fb2d`](https://github.com/szermatt/mistty/commit/977fb2d), [`1c4caa7`](https://github.com/szermatt/mistty/commit/1c4caa7), [`85a1e40`](https://github.com/szermatt/mistty/commit/85a1e40))

### Added

- Add `mistty-in-project` command to start or jump to a MisTTY buffer in the current project ([`505f664`](https://github.com/szermatt/mistty/commit/505f664))
- Support auto-completion UIs such as `corfu` and `company-mode` ([#5](https://github.com/szermatt/mistty/issues/5)) ([`e15b94e`](https://github.com/szermatt/mistty/commit/e15b94e), [`afc354c`](https://github.com/szermatt/mistty/commit/afc354c))
- Support template expansion packages such as `yasnippet` and `tempel` ([#4](https://github.com/szermatt/mistty/issues/4)) ([`6a6cecc`](https://github.com/szermatt/mistty/commit/6a6cecc), [`993c23f`](https://github.com/szermatt/mistty/commit/993c23f))

### Fixed

- Fix queued replays ([`a0b173a`](https://github.com/szermatt/mistty/commit/a0b173a))
- Fix deletion replay with non-bracketed-paste prompts ([`98f8749`](https://github.com/szermatt/mistty/commit/98f8749))
- Keep cursor at its original position after a replay ([`4deadf6`](https://github.com/szermatt/mistty/commit/4deadf6))
- Fix `mistty-send-key-sequence` (`C-c C-q`) on terminals ([`b21cb00`](https://github.com/szermatt/mistty/commit/b21cb00))

## [0.9] - 2023-09-23

_Initial release._

[2.2.0]: https://github.com/szermatt/mistty/releases/tag/2.2.0
[2.1.0]: https://github.com/szermatt/mistty/releases/tag/2.1.0
[2.0.2]: https://github.com/szermatt/mistty/releases/tag/2.0.2
[2.0.1]: https://github.com/szermatt/mistty/releases/tag/2.0.1
[1.5]: https://github.com/szermatt/mistty/releases/tag/1.5
[1.4]: https://github.com/szermatt/mistty/releases/tag/1.4
[1.3]: https://github.com/szermatt/mistty/releases/tag/1.3
[1.2]: https://github.com/szermatt/mistty/releases/tag/1.2
[1.1]: https://github.com/szermatt/mistty/releases/tag/1.1
[1.0]: https://github.com/szermatt/mistty/releases/tag/1.0
[0.9]: https://github.com/szermatt/mistty/releases/tag/0.9
