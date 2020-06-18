# Changelog

## 2.0.0
* Add `(Msg -> msg)` argument to remove the need for `Cmd.map`.

## 1.1.1
* Maintain velocity when setting target while animation is running.

## 1.1.0
* Add `isScrolling` to check if page is animating.
* Limit `isScroll` to `scene.height - viewport.height` to prevent scrolling past what is possible, resulting in smoother end of animation.

## 1.0.0
Initial release.
