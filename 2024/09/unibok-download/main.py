from urllib.parse import urlparse
import requests
import os


class Downloader:
    def __init__(self):
        self._base_url = "https://les.unibok.no"
        self._cwd = os.path.dirname(__file__)

    def download(self, name, url):
        """
        url: the URL to the opened book, for example https://les.unibok.no/#cappelendamm/p193917/2430/1
        """
        url = urlparse(url)
        fragment = url.fragment
        data = fragment.split("/")
        assert len(data) >= 3, "There is something wrong with the provided URL"
        publisher, ref, id = data[:3]

        thumbnail = requests.get(
            f"{self._base_url}/api/v1/thumbnail/publisher/{publisher}/publication/{ref}/small"
        )
        with open(os.path.join(self._cwd, f"{name}.jpg"), "wb") as file:
            file.write(thumbnail.content)

        # Requires login, so it won't work. Maybe try puppeteer?
        # Returns 403
        epub = requests.get(
            f"{self._base_url}/bookresource/publisher/{publisher}/book/{ref}/epub/{id}/offline.ub"
        )
        print(epub)
        with open(os.path.join(self._cwd, f"{name}.epub"), "wb") as file:
            file.write(epub.content)


if __name__ == "__main__":
    downloader = Downloader()
    downloader.download(
        "Enchanté", "https://les.unibok.no/#cappelendamm/p193917/2430/1"
    )
