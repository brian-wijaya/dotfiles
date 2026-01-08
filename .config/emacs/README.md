# Doom Emacs Configuration Backup

This directory contains a backup of the Doom Emacs configuration files.

## Files

- `init.el` - Module configuration (which Doom modules are enabled)
- `config.el` - Custom configuration and settings
- `packages.el` - Additional package declarations (currently empty/default)
- `WORKFLOW-GUIDE.md` - Comprehensive workflow guide with all keybindings (accessible via `SPC f w`)

## Setup Instructions

### Prerequisites

1. **Emacs 30.1+** installed (this config was tested with Emacs 30.1)
2. **Git** installed
3. **Doom Emacs** installed (see below)
4. **Windows-specific tools:**
   - Chocolatey package manager
   - `fd` (faster alternative to `find`)
   - `ripgrep` (`rg`)
   - `gsudo` (for elevated commands)

### Installation Steps

#### 1. Install Doom Emacs

If you haven't already installed Doom Emacs:

```powershell
# Clone Doom Emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs

# Add Doom to PATH (add to your PowerShell profile)
$env:PATH += ";$env:USERPROFILE\.config\emacs\bin"
```

#### 2. Install Required System Dependencies

```powershell
# Using Chocolatey (run as administrator or with gsudo)
gsudo choco install git fd ripgrep gsudo -y

# Install fonts
gsudo choco install jetbrainsmono firacode -y

# Install Harper grammar checker (optional but configured)
gsudo choco install harper -y
```

#### 3. Restore Configuration Files

Copy the configuration files to your Doom directory:

```powershell
# Backup existing config (if any)
if (Test-Path "$env:USERPROFILE\.config\doom") {
    Move-Item "$env:USERPROFILE\.config\doom" "$env:USERPROFILE\.config\doom.backup"
}

# Create doom config directory
New-Item -ItemType Directory -Path "$env:USERPROFILE\.config\doom" -Force

# Copy config files
Copy-Item "init.el" "$env:USERPROFILE\.config\doom\init.el"
Copy-Item "config.el" "$env:USERPROFILE\.config\doom\config.el"
Copy-Item "packages.el" "$env:USERPROFILE\.config\doom\packages.el"
Copy-Item "WORKFLOW-GUIDE.md" "$env:USERPROFILE\.config\doom\WORKFLOW-GUIDE.md"
```

#### 4. Initialize Doom Emacs

```powershell
# Navigate to doom directory
cd $env:USERPROFILE\.config\doom

# Run doom install (this will install all packages)
doom install --no-env

# Sync packages (this may take several minutes)
doom sync
```

#### 5. Restart Emacs

Close and restart Emacs. The configuration should now be active.

## Configuration Highlights

### Enabled Modules

**Completion:**
- `vertico` - Modern completion framework
- `corfu` - Inline completion with orderless

**UI:**
- `ligatures` - Font ligatures (→, ⇒, etc.)
- `unicode` - Extended Unicode support (Hebrew, etc.)
- `ibuffer` - Enhanced buffer management
- `eww` - Built-in web browser

**Editor:**
- `evil` - Vim keybindings
- `multiple-cursors` - Sublime-style multi-cursor editing

**Tools:**
- `(lsp +eglot)` - Language Server Protocol support
- `(lookup +dictionary)` - Dictionary and thesaurus lookup
- `tree-sitter` - Better syntax highlighting
- `editorconfig` - Respects .editorconfig files
- `vterm` - Fast terminal emulation

**Languages:**
- `(javascript +lsp)` - JavaScript/TypeScript with LSP
- `(cc +lsp)` - C/C++ with LSP
- `(go +lsp)` - Go with LSP
- `(rust +lsp)` - Rust with LSP
- `python` - Python support
- `org` - Org mode for note-taking
- `web` - HTML/CSS/web frameworks
- And many more...

### Key Customizations

1. **Fonts:**
   - Primary: JetBrains Mono
   - Fallback: Fira Code (for ligatures and Hebrew)

2. **Window Navigation:**
   - `C-h/j/k/l` for window navigation (vim-style)

3. **Org Mode:**
   - Custom agenda dashboard
   - Auto-clocking on task state changes
   - Capture templates for todos, events, projects, etc.

4. **Harper Grammar Checker:**
   - Configured for `text-mode`, `org-mode`, and `markdown-mode`
   - Requires `harper-ls` in PATH

5. **Performance Optimizations:**
   - GC threshold: 256MB
   - Deferred native compilation
   - GCMH optimizations

## Windows-Specific Notes

### fd Executable Path

The config includes a Windows-specific fix for the `fd` executable path:

```elisp
(setq doom-fd-executable "C:\\ProgramData\\chocolatey\\bin\\fd.exe")
```

If `fd` is installed elsewhere, update this path in `config.el`.

### Font Installation

Fonts are installed via Chocolatey. If you prefer manual installation:
- JetBrains Mono: https://www.jetbrains.com/lp/mono/
- Fira Code: https://github.com/tonsky/FiraCode

### Harper Grammar Checker

Harper is installed via Chocolatey. To verify installation:

```powershell
harper-ls --version
```

If not found, install it:
```powershell
gsudo choco install harper -y
```

## Troubleshooting

### Ligatures Not Showing

1. Ensure Fira Code is installed
2. Restart Emacs
3. Run `M-x doom/reload-font` in Emacs
4. Check that Emacs 28+ with Harfbuzz support is installed

### Hebrew Characters Not Displaying

1. Ensure `unicode` module is enabled in `init.el`
2. Ensure Fira Code is installed (used as fallback)
3. First startup may take 30-60 seconds to build Unicode cache
4. Check `doom-symbol-font` is set to Fira Code in `config.el`

### fd Command Not Found

1. Verify `fd` is installed: `fd --version`
2. Check PATH includes Chocolatey bin: `C:\ProgramData\chocolatey\bin`
3. Update `doom-fd-executable` path in `config.el` if installed elsewhere

### LSP Not Working

1. Ensure `(lsp +eglot)` is enabled in `init.el`
2. Install language servers for your languages:
   - TypeScript/JavaScript: `npm install -g typescript-language-server`
   - Python: `pip install python-lsp-server`
   - Rust: `rustup component add rust-analyzer`
   - Go: `go install golang.org/x/tools/gopls@latest`
3. Restart Emacs after installing language servers

### Dictionary/Thesaurus Not Working

1. Ensure `(lookup +dictionary)` is enabled in `init.el`
2. Run `doom sync` to install dictionary packages
3. Use `SPC s d t` for dictionary, `SPC s d T` for thesaurus

## Key Bindings Reference

### Navigation
- `SPC` - Leader key
- `SPC f f` - Find file
- `SPC f p` - Find file in private config
- `SPC s s` - Search in buffer
- `SPC s j` - Jump to symbol
- `C-h/j/k/l` - Navigate windows

### Org Mode
- `SPC o a` - Open agenda
- `SPC o c` - Capture
- `C-c e` - Set effort
- `C-c i` - Clock in
- `C-c o` - Clock out

### Lookup
- `SPC s d t` - Dictionary lookup
- `SPC s d T` - Thesaurus lookup

### Other
- `C-s` - Save buffer
- `C-=` - Zoom in
- `C--` - Zoom out
- `SPC f w` - Open workflow guide (comprehensive keybinding reference)
- `SPC t m` - Toggle minimap (if enabled)

## Updating Configuration

After modifying `init.el`:
```powershell
cd $env:USERPROFILE\.config\doom
doom sync
```

After modifying `config.el`:
- No sync needed, just restart Emacs or run `M-x doom/reload`

## Additional Resources

- [Doom Emacs Documentation](https://docs.doomemacs.org/)
- [Doom Emacs GitHub](https://github.com/doomemacs/doomemacs)
- [Harper Grammar Checker](https://writewithharper.com/)
- [Org Mode Manual](https://orgmode.org/manual/)

## Notes

- This configuration is optimized for Windows
- Hebrew language support is configured via Unicode module
- Ligatures require Emacs 28+ with Harfbuzz support
- First startup may be slow due to package installation and Unicode cache generation
