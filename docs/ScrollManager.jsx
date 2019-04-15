import { useEffect } from "react";
import { withRouter } from "react-router";

// Delay a callback `fn` until the returned function
// has not been called for `ms` milliseconds.
const debounceFn = (ms, fn) => {
  let timeout = null;
  return () => {
    clearTimeout(timeout);
    timeout = setTimeout(fn, ms);
  };
};

// Retry an operation every `ms` milliseconds until
// `fn` returns true or `maxTries` is reached.
const retryFn = (ms, maxTries, fn) => {
  if (fn()) return;

  const innerRetryFn = currentTry => {
    if (currentTry > maxTries) return;
    setTimeout(() => {
      if (!fn()) {
        innerRetryFn(currentTry + 1);
      }
    }, ms);
  };
  innerRetryFn(2);
};

const restoreScroll = props => {
  const { history } = props;
  switch (history.action) {
    case "PUSH":
      // New forward navigation -- scroll to top
      window.scrollTo(0, 0);
      break;
    case "POP":
      // Back or forward navigation -- attempt scroll restore if necessary
      retryFn(props.onScrollDebounceMs, props.scrollRestoreMaxRetries, () => {
        // Short circuit if there's no scroll position to restore to
        if (!window.history.state) return true;

        const { scrollX, scrollY } = window.history.state;
        if (scrollX !== window.scrollX || scrollY !== window.scrollY) {
          // Scroll position out of sync -- attempt reset and loop
          // (if a render is unusually slow the scroll attempt may fail)
          window.scrollTo(scrollX, scrollY);
          return false;
        } else {
          // Scroll position restored
          return true;
        }
      });
      break;
  }
};

const ScrollManager = props => {
  useEffect(() => {
    // Save the scroll position any time scrolling stops
    const scrollListener = debounceFn(props.onScrollDebounceMs, () => {
      const { history, scrollX, scrollY } = window;
      if (
        !history.state ||
        scrollX !== history.state.scrollX ||
        scrollY !== history.state.scrollY
      ) {
        history.replaceState({ scrollX, scrollY }, "", history.location);
      }
    });
    window.addEventListener("scroll", scrollListener);
    restoreScroll(props);
    return () => {
      window.removeEventListener("scroll", scrollListener);
    };
  }, [props.onScrollDebounceMs, props.scrollRestoreMaxRetries]);

  return props.children;
};

ScrollManager.defaultProps = {
  onScrollDebounceMs: 50,
  scrollRestoreMaxRetries: 40
};

export default withRouter(ScrollManager);
