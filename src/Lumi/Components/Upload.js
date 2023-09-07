

export const createObjectURL = function (file) {
  return URL.createObjectURL(file);
};

export const xhrUpload = function (uri, file, onProgress, onError, onComplete) {
  var xhr = new XMLHttpRequest();

  xhr.upload.addEventListener(
    "progress",
    function (e) {
      onProgress({ totalBytes: e.total | 0, uploadedBytes: e.loaded | 0 });
    },
    { passive: true }
  );

  xhr.addEventListener(
    "load",
    function (e) {
      onComplete(e.responseText);
    },
    { passive: true }
  );

  xhr.addEventListener(
    "error",
    function (e) {
      onError(e.error);
    },
    { passive: true }
  );

  xhr.open("post", uri, true);
  xhr.setRequestHeader("Content-Type", "multipart/form-data");
  xhr.send(file);
};
