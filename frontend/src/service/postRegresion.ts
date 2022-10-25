export const postData = async (arrayX: number[], arrayY: number[]) => {
  const getData = await fetch("http://localhost:8080/regresionL", {
    method: "POST", // *GET, POST, PUT, DELETE, etc.
    mode: "cors", // no-cors, *cors, same-origin
    cache: "no-cache", // *d{
    headers: {
      "Content-Type": "application/json",
      // 'Content-Type': 'application/x-www-form-urlencoded',
    },
    credentials: "same-origin",
    body: JSON.stringify({
      x: arrayX,
      y: arrayY,
    }),
  });
  return getData.json();
};

export const postDatademo = async () => {
  const getData = await fetch("http://localhost:8080/regresionL", {
    method: "POST", // *GET, POST, PUT, DELETE, etc.
    mode: "cors", // no-cors, *cors, same-origin
    cache: "no-cache", // *d{
    headers: {
      "Content-Type": "application/json",
      // 'Content-Type': 'application/x-www-form-urlencoded',
    },
    credentials: "same-origin",
    body: JSON.stringify({
      x: [
        178.0, 160.0, 183.0, 152.0, 168.0, 178.0, 188.0, 165.0, 157.0, 170.0,
        165.0, 173.0,
      ],
      y: [69.8, 67.5, 81, 60.8, 70.2, 75.6, 80.1, 72, 59.4, 65.3, 62.6, 68.4],
    }),
  });
  return getData.json();
};
