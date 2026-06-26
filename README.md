# AtrocityViz

Standalone visualization scripts and Shiny apps extracted from the main AtrocityNet project.

For online interactive visualization: https://stevenbao-atrocitynet.share.connect.posit.cloud/

![GED 2025 Network screenshot](assets/ged-2025-network-screenshot.jpeg)

To run locally, see below:

## Run locally

From Terminal, start a local web server from the project folder:

```bash
cd /Users/stvbao/Coding/AtrocityViz
python3 -m http.server 8020
```

Then open:

```text
http://localhost:8020/
```

If the project is in a different folder, replace `/Users/stvbao/Coding/AtrocityViz` with that path. If port `8020` is already in use, choose another port, for example:

```bash
python3 -m http.server 8030
```

and open `http://localhost:8030/`.
