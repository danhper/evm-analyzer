pragma solidity >=0.4.25 <0.7.0;

contract IntegerOverflow {
    mapping(address => uint8) public balances;

    constructor() public {
        balances[msg.sender] = 100;
    }

    function mintCoins(uint8 amount) public {
        uint8 minted = balances[msg.sender] + amount;
        balances[msg.sender] = minted;
    }

    function getBalance() public view returns (uint8) {
        return balances[msg.sender];
    }
}
