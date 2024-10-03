# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Fixed
- Bug where window hints would fail to convert keyword values to the appropriate hint for `client-api`, `context-creation-api`, `context-robustness`, `context-release-behavior`, and `opengl-profile`

## [1.0.86] - 2024-10-02
### Added
- Support for JDK 22+ by bumping the coffi dependency

## [0.1.66] 2021-10-16
### Added
- Add support for sections of the api
  - Initialization and Error Handling
  - Windowing
  - Monitor API
  - Input & Window Events
  - GL(ES) Context Acquisition

[Unreleased]: https://github.com/IGJoshua/glfw-clj/compare/v1.0.86...HEAD
[1.0.86]: https://github.com/IGJoshua/glfw-clj/compare/v0.1.66...v1.0.86
[0.1.66]: https://github.com/IGJoshua/glfw-clj/compare/9cfb2830924f752bad5031f1b8895ee6fba0d6cb...v0.1.66

