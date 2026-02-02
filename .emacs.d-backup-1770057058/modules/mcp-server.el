;;; mcp-server.el --- emacs MCP server -*- lexical-binding: t; -*-
(when (file-exists-p "~/.emacs.d/emacs-mcp-server.el")
  (load "~/.emacs.d/emacs-mcp-server" t)
  (when (fboundp 'emacs-mcp-server-start)
    (emacs-mcp-server-start)))
