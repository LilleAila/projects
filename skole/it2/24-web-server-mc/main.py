from flask import Flask, render_template, jsonify

app = Flask(__name__)


@app.route("/")
def home():
    return render_template("index.html")


@app.route("/restart", methods=["POST"])
def run_code():
    print("Restarting le server")
    return jsonify({"message": "Python code executed!"})


if __name__ == "__main__":
    app.run(debug=True)
