import { postData, postDatademo } from "./service/postRegresion";
import Chart from "chart.js/auto";
const buttonAdd = document.querySelector(".button-add");
const buttonDel = document.querySelector(".button-del");
const buttonSubmit = document.querySelector(".button-submit");

buttonAdd?.addEventListener("click", () => {
  const tbody = document.querySelector(".tbody");
  const tr = document.createElement("tr");
  const tdX = document.createElement("td");
  const tdY = document.createElement("td");

  const inputX = addInputs("dataX", "X");
  const inputY = addInputs("dataY", "Y");
  tdX.appendChild(inputX);
  tdY.appendChild(inputY);
  tr.appendChild(tdX);
  tr.appendChild(tdY);

  tbody?.appendChild(tr);
});
buttonDel?.addEventListener("click", () => {
  const tbody = document.querySelector(".tbody");
  if (tbody?.hasChildNodes()) {
    tbody.removeChild(tbody.lastElementChild!);
  } else {
    alert("No hay Inputs!!");
  }
});
buttonSubmit?.addEventListener("click", async () => {
  const dataX = document.querySelectorAll(".dataX");
  const dataY = document.querySelectorAll(".dataY");
  console.log(dataX);
  if (!validateEmptyInputs(dataX) && !validateEmptyInputs(dataY)) {
    const arrayX = saveData(dataX);
    const arrayY = saveData(dataY);
    const { body } = await postDatademo();
    console.log(body);
    createTable(body, body.initialDataX.length);
    createChart(body.initialDataX, body.initialDatay);
    const dataResponse = await postData(arrayX, arrayY);
    console.log(dataResponse);
  } else {
    alert("Hay campos vacios, llenalos");
  }
});

const addInputs = (
  classText: string,
  placeholder: string
): HTMLInputElement => {
  const input = document.createElement("input");
  input.type = "number";
  input.placeholder = placeholder;
  input.classList.add(
    classText,
    "drac-input",
    "drac-input-purple",
    "drac-text-purple"
  );
  return input;
};

const validateEmptyInputs = (arrayData: NodeList): boolean =>
  [...[arrayData]].some(
    (data, index) => (data[index] as HTMLInputElement).value === ""
  );

const saveData = (arrayData: NodeList): number[] => {
  const arr: number[] = [];

  arrayData.forEach((el) => {
    arr.push(Number((el as HTMLInputElement).value));
  });

  return arr;
};

const createTable = (data: any, len: number) => {
  const tbody = document.querySelector(".tbody-results");
  const tbody2 = document.querySelector(".tbody-results2");
  const table = document.querySelectorAll(".table");
  const divTable = document.querySelector("#tabla");
  const fragment = new DocumentFragment();

  tbody!.innerHTML = "";
  tbody2!.innerHTML = "";
  table[0]?.classList.remove("hide");
  table[0]?.classList.add("show");
  table[1]?.classList.remove("hide");
  table[1]?.classList.add("show");

  for (let index = 0; index < len; index++) {
    const tr = document.createElement("tr");
    tr.innerHTML = `
      <td class='drac-text drac-text-white'> ${data.initialDataX[index].toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-white'> ${data.initialDatay[index].toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-white'> ${data.valuesMinusX[index].toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-white'> ${data.valuesMinusY[index].toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-white'> ${data.powValuesX[index].toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-white'> ${data.powValuesY[index].toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-white'> ${data.multiplyXY[index].toFixed(
        2
      )} </td>
    `;
    fragment?.appendChild(tr);
  }
  const tr = document.createElement("tr");
  tr.innerHTML = `
      <td class='drac-text drac-text-black drac-bg-green'> ${data.dataX.toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-black drac-bg-green'> ${data.dataY.toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-black drac-bg-green'> N/A </td>
      <td class='drac-text drac-text-black drac-bg-green'> N/A </td>
      <td class='drac-text drac-text-black drac-bg-green'> ${data.totalPowValX.toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-black drac-bg-green'> ${data.totalPowValY.toFixed(
        2
      )} </td>
      <td class='drac-text drac-text-black drac-bg-green'> ${data.totalXY.toFixed(
        2
      )} </td>
    `;

  tbody?.appendChild(fragment);
  tbody?.appendChild(tr);

  const tableDataResults = document.createElement("table");
  tableDataResults.classList.add("drac-table");
  tbody2!.innerHTML = `
          <tr >
          <td class="drac-text drac-text-white">${data.mediumX.toFixed(2)}</td>
          <td class="drac-text drac-text-white">${data.mediumY.toFixed(2)}</td>
          <td class="drac-text drac-text-white">${data.sx.toFixed(2)}</td>
          <td class="drac-text drac-text-white">${data.sy.toFixed(2)}</td>
          <td class="drac-text drac-text-white">${data.sxy.toFixed(2)}</td>
          <td class="drac-text drac-text-white">${data.r.toFixed(2)}</td>
          <td class="drac-text drac-text-white">${data.relation.toFixed(2)}</td>
          </tr>
  `;

  divTable?.appendChild(tableDataResults);
};

const createChart = (dataX: number[], dataY: number[]) => {
  const canvas = <HTMLCanvasElement>document.getElementById("myChart");
  const ctx = canvas.getContext("2d");
  const unitedData: any = [];
  for (let index = 0; index < dataX.length; index++) {
    unitedData.push({ x: dataX[index], y: dataY[index] });
  }

  console.table(unitedData);
  const data = {
    datasets: [
      {
        label: "Regresion Lineal",
        data: unitedData,
        backgroundColor: "rgb(255, 99, 132)",
        borderColor: "rgb(134, 13, 45)",
      },
    ],
  };
  const config = {
    type: "scatter",
    data: data,
    options: {
      scales: {
        x: {
          type: "linear",
          position: "bottom",
        },
      },
    },
  };

  new Chart(ctx!, config);
  canvas.classList.remove("hide");
  canvas.classList.add("show");
};
