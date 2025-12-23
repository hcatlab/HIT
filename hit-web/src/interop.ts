// Persists to localStorage under the 'hit' key

export const flags = async () => {
  try {
    const raw = localStorage.getItem('hit');
    return raw ? JSON.parse(raw) : {};
  } catch {
    return {};
  }
};

export const onReady = ({ app }: { app: { ports?: Record<string, { subscribe?: (cb: (data: any) => void) => void }> } }) => {
  const sendToLocalStorage = app?.ports?.sendToLocalStorage;
  if (sendToLocalStorage?.subscribe) {
    sendToLocalStorage.subscribe(({ key, value }: { key: string; value: any }) => {
      try {
        if (value === null) {
          localStorage.removeItem(key);
        } else {
          localStorage.setItem(key, JSON.stringify(value));
        }
      } catch {
        /* ignore */
      }
    });
  }
};
