<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-g">
    <title>Haskell Stock Sim</title>
    <style>
        body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; display: flex; gap: 20px; padding: 20px; background-color: #f0f0f0; }
        div { background-color: #fff; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        h2 { padding: 10px 20px; border-bottom: 1px solid #eee; margin-top: 0; }
        table { border-collapse: collapse; width: 100%; }
        th, td { padding: 10px 20px; text-align: left; border-bottom: 1px solid #eee; }
        #stocks td:nth-child(2) { font-weight: bold; }
        button { cursor: pointer; padding: 5px 10px; border: none; border-radius: 4px; background-color: #007aff; color: white; }
        #portfolio-cash { font-size: 1.2em; font-weight: bold; color: #333; padding: 10px 20px; }
        #trades { max-height: 200px; overflow-y: auto; }
        #trades p { padding: 5px 20px; margin: 0; border-bottom: 1px solid #eee; }
    </style>
</head>
<body>

    <div id="market">
        <h2>Market</h2>
        <table id="stocks">
            <thead><tr><th>ID</th><th>Price</th><th>Actions (Qty 1)</th></tr></thead>
            <tbody>
                </tbody>
        </table>
    </div>

    <div>
        <div id="portfolio">
            <h2>Portfolio</h2>
            <div id="portfolio-cash">Cash: $10000.00</div>
            <table id="holdings">
                <thead><tr><th>ID</th><th>Shares</th></tr></thead>
                <tbody></tbody>
            </table>
        </div>
        <div id="trades">
            <h2>Recent Trades</h2>
            <div id="trades-list"></div>
        </div>
    </div>

    <script>
        const ws = new WebSocket("ws://127.0.0.1:9160");

        const stocksBody = document.querySelector("#stocks tbody");
        const cashDiv = document.querySelector("#portfolio-cash");
        const holdingsBody = document.querySelector("#holdings tbody");
        const tradesList = document.querySelector("#trades-list");

        // Object để lưu trữ trạng thái
        let stockState = {};

        ws.onopen = () => {
            console.log("Connected to Haskell server");
        };

        // Nhận tin nhắn từ server
        ws.onmessage = (event) => {
            const msg = JSON.parse(event.data);
            const tag = Object.keys(msg)[0];
            const data = msg[tag];

            switch (tag) {
                case "FullStockUpdate":
                    updateStockTable(data);
                    break;
                case "PriceUpdate":
                    updateStockPrice(data);
                    break;
                case "PortfolioUpdate":
                    updatePortfolio(data);
                    break;
                case "TradeLogUpdate":
                    updateTradeLog(data);
                    break;
            }
        };

        function updateStockTable(stocks) {
            stockState = stocks;
            stocksBody.innerHTML = "";
            for (const id in stocks) {
                const stock = stocks[id];
                const row = document.createElement("tr");
                row.id = `stock-${id}`;
                row.innerHTML = `
                    <td>${id}</td>
                    <td>${stock.sPrice.toFixed(2)}</td>
                    <td>
                        <button onclick="sendOrder('${id}', 'Buy')">Buy</button>
                        <button onclick="sendOrder('${id}', 'Sell')">Sell</button>
                    </td>
                `;
                stocksBody.appendChild(row);
            }
        }

        function updateStockPrice(priceEvent) {
            const row = document.getElementById(`stock-${priceEvent.peStock}`);
            if (row) {
                row.cells[1].textContent = priceEvent.pePrice.toFixed(2);
                row.cells[1].style.backgroundColor = '#dfffea'; // Tô màu khi cập nhật
                setTimeout(() => { row.cells[1].style.backgroundColor = 'transparent'; }, 500);
            }
        }

        function updatePortfolio(portfolio) {
            cashDiv.textContent = `Cash: $${portfolio.cash.toFixed(2)}`;
            holdingsBody.innerHTML = "";
            for (const id in portfolio.holdings) {
                const row = document.createElement("tr");
                row.innerHTML = `<td>${id}</td><td>${portfolio.holdings[id]}</td>`;
                holdingsBody.appendChild(row);
            }
        }

        function updateTradeLog(trades) {
            tradesList.innerHTML = "";
            trades.forEach(trade => {
                const p = document.createElement("p");
                p.textContent = `[${trade.tType}] ${trade.tStock} x ${trade.tQty} @ ${trade.tPrice.toFixed(2)}`;
                tradesList.appendChild(p);
            });
        }

        // Gửi lệnh đến server
        function sendOrder(stockId, orderType) {
            const msg = {
                tag: "PlaceOrder",
                oStock: stockId,
                oType: orderType,
                oQty: 1 // Gửi cố định số lượng 1
            };
            ws.send(JSON.stringify(msg));
            console.log("Sent order:", msg);
        }

        ws.onclose = () => {
            console.log("Disconnected from server");
            document.body.innerHTML = "<h1>Disconnected from server. Please refresh.</h1>";
        };
    </script>
</body>
</html>