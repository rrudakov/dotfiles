package `(mapconcat 'identity (split-string (replace-regexp-in-string ".*src\\(/\\(main\\|test\\)\\)?\\(/java\\)?" "" default-directory) "/" t) ".")`;

public class `(file-name-base
               (or (buffer-file-name)
                   (buffer-name)))` {
  $0
}
