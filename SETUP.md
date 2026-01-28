# Fresh System Setup

Arch Linux. Two repos: `~/dotfiles/` (machine config) and `~/vault/` (knowledge + programs).

For architectural decisions, see `~/vault/docs/adr/`.

---

## Part 1: Human Steps (do these yourself)

You need to bootstrap far enough that Claude Code can take over. That means: base system, repos cloned, git-crypt unlocked, Claude running with sudo access.

### 1. Install Arch Linux

Minimal install. Packages to include:
```
pacstrap /mnt base linux linux-firmware networkmanager git zsh sudo nodejs npm python
```

Create user and set shell:
```
useradd -m -G wheel -s /usr/bin/zsh bw
passwd bw
```

Uncomment `%wheel ALL=(ALL:ALL) ALL` in `/etc/sudoers` via `visudo`.

Enable networking:
```
systemctl enable NetworkManager
```

Reboot into the new system and log in as `bw`.

### 2. Generate SSH key and add to GitHub

```
ssh-keygen -t ed25519 -C "bw1animation@gmail.com"
cat ~/.ssh/id_ed25519.pub
```

Go to https://github.com/settings/keys and add the public key.

### 3. Clone repos

```
git clone git@github.com:brian-wijaya/dotfiles.git ~/dotfiles
git clone git@github.com:retopo001/vault.git ~/vault
```

### 4. Install git-crypt and unlock dotfiles

Some files in dotfiles are encrypted (Emacs tokens, Claude settings, email credentials). You need the git-crypt key from 1Password to decrypt them.

Install git-crypt:
```
sudo pacman -S git-crypt
```

Find your new machine's IP address (you'll need this to receive the key):
```
ip addr show | grep 'inet ' | grep -v 127.0.0.1
```
The address looks like `192.168.x.x` or `10.x.x.x`.

Enable SSH so you can receive the file:
```
sudo pacman -S openssh
sudo systemctl start sshd
```

From your phone or another device with 1Password:
1. Open 1Password, find the git-crypt key file (`dotfiles-git-crypt.key`)
2. Download it
3. Send it to the new machine (replace `YOUR_IP` with the address from above):
   ```
   scp dotfiles-git-crypt.key bw@YOUR_IP:~/
   ```
   If scp isn't available on the sending device, use any file transfer method — email it to yourself, AirDrop to a laptop, whatever gets the file to `~/` on the new machine.

Unlock and delete the key:
```
cd ~/dotfiles
git-crypt unlock ~/dotfiles-git-crypt.key
rm ~/dotfiles-git-crypt.key
```

Verify — this should show readable email config, not binary gibberish:
```
head -3 ~/dotfiles/.mbsyncrc
```

### 5. Deploy Claude Code config and sudo MCP

Claude needs its config and the sudo MCP server before it can install anything.

```
cp ~/dotfiles/.claude.json ~/.claude.json
mkdir -p ~/.claude
cp ~/dotfiles/.claude/settings.json ~/.claude/settings.json
cp ~/dotfiles/.claude/settings.local.json ~/.claude/settings.local.json
cp -r ~/dotfiles/.claude/skills ~/.claude/skills
cp ~/dotfiles/CLAUDE.md ~/CLAUDE.md
```

Set up the sudo MCP server (just needs python3, already installed):
```
cd ~/vault/programs/sudo-mcp
python -m venv .venv
.venv/bin/pip install -e .
```

### 6. Install Claude Code

```
npm install -g @anthropic-ai/claude-code
```

### 7. Install a terminal emulator and tmux

You need a terminal to run Claude. Ghostty is the default, but any terminal works for bootstrapping:
```
sudo pacman -S ghostty tmux
```

If ghostty isn't available in pacman yet, use alacritty:
```
sudo pacman -S alacritty tmux
```

### 8. Launch Claude Code

Open your terminal, start a tmux session, and run Claude:
```
tmux new -s main
claude
```

Claude will start with `~/.claude.json` loaded, giving it access to the sudo MCP server.

### 9. Tell Claude: **"Set up the rest from SETUP.md"**

Claude takes over from here. It will install all packages, deploy all configs, build all MCP servers, and verify the setup.

---

## Part 2: LLM Steps (Claude does the rest)

Everything below should be executed by Claude. The sequencing matters — each phase unlocks the next. Claude already has sudo access via the sudo MCP deployed in Part 1.

### Phase 1: System Packages

```
sudo pacman -S --needed - < ~/dotfiles/archlinux/packages.txt
```

Install yay (AUR helper):
```
cd /tmp
git clone https://aur.archlinux.org/yay.git
cd yay && makepkg -si --noconfirm
```

AUR packages:
```
yay -S --noconfirm xidlehook i3lock-color clipmenu satty google-chrome sddm-theme-tokyo-night-git
```

### Phase 2: Shell Bootstrap

Copy in this order (`.zshenv` first — it sets PATH and XDG dirs for everything else):
```
cp ~/dotfiles/.zshenv ~/.zshenv
cp ~/dotfiles/.bashrc ~/.bashrc
cp ~/dotfiles/.bash_profile ~/.bash_profile
cp ~/dotfiles/.zshrc ~/.zshrc
```

Authenticate GitHub CLI (needed for token extraction in `.zshenv`):
```
gh auth login
```

### Phase 3: Git and Email

```
cp ~/dotfiles/.gitconfig ~/.gitconfig
cp ~/dotfiles/.mbsyncrc ~/.mbsyncrc
```

### Phase 4: X11 and Window Manager

```
mkdir -p ~/.config
cp ~/dotfiles/.xinitrc ~/.xinitrc
cp ~/dotfiles/.XCompose ~/.XCompose
cp ~/dotfiles/.xbindkeysrc ~/.xbindkeysrc
cp -r ~/dotfiles/.config/i3 ~/.config/i3
cp -r ~/dotfiles/.config/picom ~/.config/picom
cp -r ~/dotfiles/.config/polybar ~/.config/polybar
cp -r ~/dotfiles/.config/rofi ~/.config/rofi
cp -r ~/dotfiles/.config/dunst ~/.config/dunst
cp -r ~/dotfiles/.config/gtk-3.0 ~/.config/gtk-3.0
cp -r ~/dotfiles/.config/wallpaper ~/.config/wallpaper
chmod +x ~/.config/polybar/launch.sh
```

Enable display manager:
```
sudo systemctl enable sddm
```

### Phase 5: Terminal and Editor Configs

```
cp -r ~/dotfiles/.config/alacritty ~/.config/alacritty
cp -r ~/dotfiles/.config/ghostty ~/.config/ghostty
cp -r ~/dotfiles/.config/wezterm ~/.config/wezterm
cp -r ~/dotfiles/.config/fish ~/.config/fish
cp -r ~/dotfiles/.config/readline ~/.config/readline
cp ~/dotfiles/.config/starship.toml ~/.config/starship.toml
cp -r ~/dotfiles/.config/tmux ~/.config/tmux
cp -r ~/dotfiles/.tmuxifier ~/.tmuxifier
cp -r ~/dotfiles/.config/yazi ~/.config/yazi
cp -r ~/dotfiles/.config/nvim ~/.config/nvim
```

### Phase 6: Emacs

```
cp -r ~/dotfiles/.emacs.d ~/.emacs.d
mkdir -p ~/.config/emacs
cp ~/dotfiles/.config/emacs/config.el ~/.config/emacs/config.el
cp ~/dotfiles/.config/emacs/init.el ~/.config/emacs/init.el
cp ~/dotfiles/.config/emacs/packages.el ~/.config/emacs/packages.el
```

Secrets (`.emacs.d/secrets.el`) are already decrypted and in place from the `cp -r`.

Emacs will install packages automatically on first launch.

### Phase 7: Scripts and Desktop Entries

```
cp -r ~/dotfiles/bin ~/bin
chmod +x ~/bin/*
mkdir -p ~/.local/share
cp -r ~/dotfiles/.local/share/applications ~/.local/share/applications
cp -r ~/dotfiles/.local/share/icons ~/.local/share/icons
```

### Phase 8: Systemd User Services

```
cp -r ~/dotfiles/.config/systemd ~/.config/systemd
systemctl --user daemon-reload
systemctl --user enable vault-rag-watcher.service
```

### Phase 9: Python MCP Servers

Each server needs its own venv. sudo-mcp is already set up from Part 1.

```
cd ~/vault/programs/vault-rag
python -m venv .venv
.venv/bin/pip install -e .

cd ~/vault/programs/claude-in-emacs
python -m venv .venv
.venv/bin/pip install -e .

cd ~/vault/programs/x11-mcp
python -m venv .venv
.venv/bin/pip install -e .
```

### Phase 10: Compiled MCP Servers

Somatic perception suite (C++20, requires xcb, cairo, libxxhash, xkbcommon):
```
cd ~/vault/programs/bw-mcp-somatic
meson setup builddir
meson compile -C builddir
sudo meson install -C builddir
```

This installs `somatic-*` binaries to `/usr/local/bin/`.

Claude coordinator (Go):
```
cd ~/vault/programs/claude-coordinator
go build -o bin/coordinator ./cmd/coordinator
```

### Phase 11: Verify

1. Reboot into SDDM, log in, i3 starts
2. Open ghostty, tmux attaches, zsh with starship prompt
3. Run `claude` — all MCP servers should connect
4. Run `/loose-config-search` — should report no drift

---

## What's Encrypted (git-crypt)

| File | Contents |
|------|----------|
| `.emacs.d/secrets.el` | Emacs auth tokens |
| `.claude/settings.json` | Claude Code settings |
| `.mbsyncrc` | Email credentials |

Key must be available at unlock time. Back it up separately.

## What's NOT in Dotfiles

- `~/vault/` — separate repo, separate clone
- `~/.authinfo` — regenerate fresh credentials on new system
- `~/.ssh/` — generate new keys, add to services
- Package caches, compiled artifacts, history files — all regenerated
