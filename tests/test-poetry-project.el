(require 'ert)

(require 'el-mock)

(require 'poetry-project)


;;; Test `poetry-project--root'.
(ert-deftest test-poetry-project--root-in-project ()
  (with-mock
    (mock
     (locate-dominating-file "/home/user/project/src" "pyproject.toml")
     => "/home/user/project")
    (stub insert-file-contents-literally)
    (stub buffer-string => "[tool.poetry]")
    (should (equal (poetry-project--root "/home/user/project/src") "/home/user/project"))))

(ert-deftest test-poetry-project--root-no-project ()
  (with-mock
      (mock
       (locate-dominating-file "/home/user/project/src" "pyproject.toml")
       => nil)
      (stub insert-file-contents-literally)
      (stub buffer-string => "[tool.poetry]")
      (should-not (poetry-project--root "/home/user/project/src"))))

;;; Test `poetry-project--name'.
(ert-deftest test-poetry-project--name--poetry-toml ()
  "Test that a valid Poetry toml file returns the extracted project name."
  (let* ((project-root "/usr/home/some-user/projects/foo")
         (pyproject-path (concat project-root "/pyproject.toml")))
    (with-mock
      (mock (insert-file-contents pyproject-path))
      (mock (re-search-forward * * *) => t)
      (mock (match-string 1) => "project-name")
      (should (equal
               (poetry-project--name project-root) "project-name")))))

(ert-deftest test-poetry-project--name--random-toml ()
  "Test that some non-Poetry toml file throws an error."
  (let* ((project-root "/usr/home/some-user/projects/foo")
         (pyproject-path (concat project-root "/pyproject.toml")))
    (with-mock
      (mocklet (((insert-file-contents pyproject-path))
                ((re-search-forward * * *) => nil)
                (match-string not-called))
        (should-error (poetry-project--name project-root))))))
