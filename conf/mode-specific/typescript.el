;;; TypeScript. -*- lexical-binding: t -*-

(use-package typescript-mode
  :ensure t)

;; The `typescript' npm package provides a `tsserver' binary that speaks JSON, but this is not actually an LSP; it uses a custom protocol. (However, once TypeScript 7 is released, it's likely to speak LSP.)
;; Our options are:
;; 1. Use a wrapper from <https://github.com/typescript-language-server/typescript-language-server> (formerly called theia-ide). This is the setup recommended in lsp-mode's docs.
;; 2. Use the `tide' package, which was made to provide language features in the pre-LSP era, but is still maintained. It speaks `tsserver's protocol directly.
;; (It's redundant to use tide and LSP at the same time.)

;; Useful config options for option 1:
;; - `lsp-clients-typescript-prefer-use-project-ts-server' - set this to `t' to prefer the project's local typescript-language-server over your global one.
;; - `lsp-clients-typescript-tls-path' - set the path manually.
;; See <https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/> for more.

;; The TypeScript LSP server also works great for JavaScript. Just add a jsconfig.json instead of a tsconfig.json and continue writing plain JS.

;; If you want to make Emacs work with the project's locally installed eslint, see <https://www.rahuljuliato.com/posts/eslint-on-emacs>.

;; To work on old projects that use Node 12 or earlier, install TypeScript and the server globally with a higher version:
;;    nvm use 14
;;    npm install -g typescript-language-server@3.3.2 typescript@5.0.4
;; Then start Emacs from a shell that has had "nvm use 14" executed in it.

(provide 'conf/mode-specific/typescript)
