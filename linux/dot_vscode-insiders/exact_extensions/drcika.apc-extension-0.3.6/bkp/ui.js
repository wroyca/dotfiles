function onDidFilesChange(event) {
  store.watchedFiles.size && [event.rawUpdated, event.rawAdded, event.rawDeleted].some(changes => changes.some(config => store.watchedFiles.has(config.path))) && appendFiles();
  store.watchedIframeStyle && [event.rawUpdated, event.rawAdded, event.rawDeleted].some(changes => changes.some(config => store.watchedIframeStyle.path === config.path)) && appendIframeStyles();
}

const WORKSPACE = 1; // ?? enum StorageScope
const USER = 0; // ?? enum StorageTarget

async function appendIframeStyles() {
  try {
    const savedIframeImport = services.storageService.get('apc.iframe.style', WORKSPACE);
    const iframeImport = config.getConfiguration('apc.iframe.style');

    if (!savedIframeImport && !iframeImport) { return; }

    const appRoot = services.environmentService.appRoot;
    const IFrameIndexPath = uri.URI.parse('file://' + appRoot + '/out/vs/workbench/contrib/webview/browser/pre/index.html');
    const IFrameIndexPathBkpPath = uri.URI.parse('file://' + appRoot + '/out/vs/workbench/contrib/webview/browser/pre/index.apc.bkp.html');

    const backup = await services.fileService.exists(IFrameIndexPathBkpPath);
    if (!backup) {
      await services.fileService.copy(IFrameIndexPath, IFrameIndexPathBkpPath);
    }

    store.watchedIframeStyle?.disposable?.dispose?.();
    store.watchedIframeStyle = undefined;

    if (savedIframeImport && !iframeImport) {
      // revert
      services.fileService.copy(IFrameIndexPathBkpPath, IFrameIndexPath);
      services.storageService.remove('apc.iframe.style', WORKSPACE);
      return;
    }

    const iframeIndexRaw = await services.fileService.readFile(IFrameIndexPathBkpPath);
    const URI = uri.URI.parse(!iframeImport.startsWith('file://') ? 'file://' + iframeImport : iframeImport);
    const styleRaw = await services.fileService.readFile(URI);
    const iframeIndex = iframeIndexRaw?.value?.toString?.() || '';
    const style = styleRaw?.value?.toString?.() || '';

    if (iframeIndex && style) {
      const patchedIframe = iframeIndex
        .replace('meta http-equiv="Content-Security-Policy"', 'meta http-equiv=""')
        .replace('blockquote {', `\t${style}\n\tblockquote {`);

      services.fileService.writeFile(IFrameIndexPath, services.VSBuffer.fromString(patchedIframe));
      services.storageService.store('apc.iframe.style', iframeImport, WORKSPACE, USER);

      const disposable = services.fileService.watch(URI);
      config.disposables.add(disposable);
      store.watchedIframeStyle = { path: URI.path, disposable };
    }

  } catch (err) { traceError(err); }
}


require(['vs/base/common/buffer'], classes.buffer, traceError);
exports.buffer = function (buffer) {
  try {
    const [, VSBuffer] = findOwnProperty(buffer, 'VSBuffer', 'fromString');
    services.VSBuffer = VSBuffer;
  } catch (error) { traceError(error); }
};
