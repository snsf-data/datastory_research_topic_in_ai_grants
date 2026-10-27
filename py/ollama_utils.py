# ---------------------------------------------------------------------------- #
#                                    Setup                                 -----
# ---------------------------------------------------------------------------- #

import subprocess
import time
import requests

# ---------------------------------------------------------------------------- #
#                                Ollama utils                              -----
# ---------------------------------------------------------------------------- #
# The function in this section are utilities used to start/stop Ollama local
# server and are not exported. None of the function are exported by default, but
# they can be accessed with
# `from findex.abstract.ollama_utils import stop_ollama`.


def start_ollama():
    """Start the Ollama local server.

    The function tests if the Ollama server is responding and launch when it is
    not responding. The function takes no parameter and does not return
    anything, it used only for side-effect.
    """

    # Check if the Ollama local server is responding. If the request raises an
    # error, it means the app is not running.
    try:
        requests.get("http://localhost:11434/api/tags", timeout=0.5)
        return
    except:  # noqa: E722
        pass

    # Start Ollama server manually (using the approach `subprocess.Popen()` is
    # the best way to start a background process).
    subprocess.Popen(
        ["ollama", "serve"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    # Give the server some time to start
    time.sleep(2)


def stop_ollama():
    """Stop the Ollama local server.

    The function tests if the Ollama server is responding and kills it if it is
    responding. The function takes no parameter and does not return anything, it
    is used only for side-effect.
    """

    # Check if the Ollama local server is responding. If the request raises an
    # error, it means the app is not running and no action is thus needed.
    try:
        requests.get("http://localhost:11434/api/tags", timeout=0.5)
        subprocess.run(
            "taskkill /IM ollama.exe /F",
        )  # noqa: F841
    except:  # noqa: E722
        return
