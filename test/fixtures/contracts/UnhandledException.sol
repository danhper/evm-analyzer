pragma solidity >=0.4.25 <0.7.0;

contract UnhandledException {
    address payable constant theVoid = 0x0000000000000000000000000000000000000000;

    event Sent(uint256 amount);

    function sendToNull(uint256 amount) public {
        theVoid.send(amount);
        if (amount > 0) {
            emit Sent(amount);
        }
    }
}
